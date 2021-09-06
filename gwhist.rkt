#lang rackjure
(require games/cards
         racket/gui/base
         racket/class)

(require "util.rkt")

(current-curly-dict hasheq)

;;;; Preliminary stuff

(define game-name "German Whist")
(define game-version "1.0")
(define version-message
  (format #<<version
~a ~a
https://github.com/erkin/german-whist

Copyright (C) 2021 Lulu Cathrinus Grimalkin
Each file of this project's source code is subject 
to the terms of the Mozilla Public Licence v2.0
https://mozilla.org/MPL/2.0/
version
          game-name game-version))

;;;; Windowing definitions

;;; Dimensions in terms of cards (71x96)
(define table-w 10)
(define table-h 5)

(define table
  (make-table game-name
              table-w
              table-h))

(define (quit!)
  (send table show #f)
  (custodian-shutdown-all (current-custodian))
  (queue-callback exit #t))

;; TODO
(define (about)
  (message-box
   (format "About ~A v~A" game-name game-version)
   version-message table '(ok no-icon)))

(define status-pane (send table create-status-pane))

(void
 (new button% (parent status-pane)
      (label "About")
      (callback (thunk* (about))))
 (new button% (parent status-pane)
      (label "Exit")
      (callback (thunk* (quit!)))))

;;;; Card definitions

(define pack
  (map (λ (card)
         (send* card
           (user-can-flip #f)
           (user-can-move #f))
         card)
       (shuffle-list (make-deck) 7)))

(define (card>? a b)
  (let ((a-suit  (send a get-suit-id))
        (a-value (send a get-value))
        (b-suit  (send b get-suit-id))
        (b-value (send b get-value)))
    ;; Higher suit (for the sake of sorting)
    (or (> a-suit b-suit)
        (and (= a-suit b-suit)
             ;; Aces rank above others.
             (or (= a-value 1)
                 ;; Higher rank otherwise
                 (and (> b-value 1)
                      (> a-value b-value)))))))

(define (beats? lead follow trump-suit)
  (let ((lead-suit    (send lead get-suit))
        (lead-value   (send lead get-value))
        (follow-suit  (send follow get-suit))
        (follow-value (send follow get-value)))
    (if (eq? lead-suit follow-suit)
        (or (= lead-value 1)
            (and (> follow-value 1)
                 (> lead-value follow-value)))
        (not (eq? follow-suit trump-suit)))))

(define (pick-a-card state)
  (let* ((turn (dict-ref state 'turn))
         (card-led (dict-ref state 'card-led))
         (hand (dict-ref-in state `(,turn cards))))
    (define playables
      (cond (card-led
             (define followers
               (filter
                (λ (card)
                  (= (send card get-suit-id)
                     (send card-led get-suit-id)))
                hand))
             (if (null? followers)
                 hand
                 followers))
            (else
             hand)))
    (case turn
      ((player)
       (define player-region (dict-ref-in state '(player region)))
       (define selection (box #f))
       (define sema (make-semaphore))
       (for-each (λ (c) (send c dim #f)) playables)
       (send table set-single-click-action
             (λ (c)
               (send table pause 0.1)
               (when (member c playables)
                 (send table set-single-click-action void)
                 (set-box! selection c)
                 (semaphore-post sema))))
       (yield sema)
       (unbox selection))
      (else
       (sleep/yield 0.1)
       (list-random-ref playables)))))

;;;; Region and hand definitions

(define w (send table table-width))
(define h (send table table-height))

(define cw (send (car pack) card-width))
(define ch (send (car pack) card-height))

(define-values (player-region opponent-region talon-region player-field opponent-field player-pile opponent-pile)
  (let* ((mid-h (quotient (- h ch) 2))
         (mid-w (quotient (- w cw) 2))
         ;;                             X coord   Y coord    width  height  label
         (talon-region    (region             0     mid-h       cw      ch  #f #f))
         (player-region   (region      (/ cw 2)  (- h ch)  (- w cw)     ch  #f #f))
         (opponent-region (region      (/ cw 2)         0  (- w cw)     ch  #f #f))
         (player-field    (region  (- mid-w cw)     mid-h       cw      ch  #f #f))
         (opponent-field  (region  (+ mid-w cw)     mid-h       cw      ch  #f #f))
         (player-pile     (region      (- w cw)  (* 3 ch)       cw      ch  #f #f))
         (opponent-pile   (region      (- w cw)        ch       cw      ch  #f #f)))
    (values
     player-region
     opponent-region
     talon-region
     player-field
     opponent-field
     player-pile
     opponent-pile)))


;;;; Gameplay definitions

(define (next-player player)
  (if (eq? player 'player)
      'opponent
      'player))

(define (prepare-table! state)
  (define-values (talon-cards player-cards opponent-cards)
    (apply values (map (curryr dict-ref 'cards) (dict-refs state '(talon player opponent)))))  
  (send* table
    (set-single-click-action void)
    (set-double-click-action void)
    (set-button-action 'left 'drag-raise/one)
    ;; Trump suit indicator
    (add-region (region 0 (* ch 3) cw 20 (symbol->string (dict-ref state 'trump)) #f))
    ;; The talon on the side
    (add-region talon-region)
    (add-cards-to-region talon-cards talon-region)
    ;; Player's visible cards
    (add-region player-region)
    (add-cards-to-region player-cards player-region)
    (cards-face-up player-cards)
    ;; Bot's hidden cards
    (add-region opponent-region)
    (add-cards-to-region opponent-cards opponent-region)
    ;; Trick and pile regions
    (add-region player-field)
    (add-region player-pile)
    (add-region opponent-field)
    (add-region opponent-pile)
    (center)
    (show #t))
  (sort-cards! state))

(define (sort-cards! state)
  (define player-hand (sort (dict-ref-in state '(player cards)) (negate card>?)))
  (define opponent-hand (dict-ref-in state '(opponent cards)))
  (for-each (λ (c) (send c dim #f)) (send table all-cards))
  (for-each (λ (c) (send c dim #t)) player-hand)
  (send table move-cards-to-region player-hand (dict-ref-in state '(player region)))
  (send table stack-cards player-hand)
  (send table move-cards-to-region opponent-hand (dict-ref-in state '(opponent region)))
  (send table stack-cards opponent-hand))

(define (lead state)
  (define player (dict-ref state 'turn))
  (cond
    ;; Talon depleted, we move to the second stage of the game.
    ((null? (dict-ref-in state '(talon cards)))
     (game-loop (dict-set state 'phase 'lead*)))
    ;; Talon not depleted, still in the first stage of the game.
    (else
     ;; Flip the top card of the talon.
     (send table card-face-up (car (dict-ref-in state '(talon cards))))
     ;; What should the player lead?
     (define lead (pick-a-card state))
     ;; Flip the card up.
     (send table card-face-up lead)
     ;; Move the card to the player's trick region on the table.
     (send table move-cards-to-region (list lead) (dict-ref-in state `(,player trick-region)))
     (game-loop (~> state
                    ;; Remove the card led from the player's hand.
                    (dict-update-in `(,player cards) #λ(remove lead %))
                    ;; Put the card on the table.
                    (dict-set 'card-led lead)
                    ;; It's the next player's turn to follow to the trick.
                    (dict-update 'turn next-player)
                    (dict-set 'phase 'follow))))))

(define (follow state)
  (define player (dict-ref state 'turn))
  (define lead (dict-ref state 'card-led))
  (define follow (pick-a-card state))
  ;; Who wins the trick?
  (define winner (if (beats? lead follow (dict-ref state 'trump))
                     (next-player player)
                     player))
  ;; Turned up card on the talon.
  (define winners-card (car  (dict-ref-in state '(talon cards))))
  ;; The next card on the talon.
  (define losers-card  (cadr (dict-ref-in state '(talon cards))))
  (send table card-face-up winners-card)
  ;; Flip the played card up.
  (send table card-face-up follow)
  ;; Move the card to the player's trick region on the table.
  (send table move-cards-to-region (list follow) (dict-ref-in state `(,player trick-region)))
  (send table card-face-up (car (dict-ref-in state '(talon cards))))
  ;; Collect the trick.
  (sleep/yield 0.1)
  (send table move-cards-to-region (list lead follow) (dict-ref-in state `(,winner pile-region)))
  ;; Flip the cards over.
  (send table cards-face-down (list lead follow))
  ;; Move the top card of the talon to the winner's hand.
  (send table move-cards-to-region (list winners-card) (dict-ref-in state `(,winner region)))
  ;; Move the next card to the loser's hand.
  (send table move-cards-to-region (list losers-card) (dict-ref-in state `(,(next-player winner) region)))
  ;; Player's cards must remain visible and the opponent's cards must remain invisible.
  (if (eq? winner 'player)
      (send* table
        (card-face-up winners-card)
        (card-face-down losers-card))
      (send* table
        (card-face-up losers-card)
        (card-face-down winners-card)))
  (game-loop (~> state
                 ;; Remove the played card from the player's hand.
                 (dict-update-in `(,player cards) (curry remove follow))
                 ;; Pick the card off the table.
                 (dict-set 'card-led #f)
                 ;; The winner picks up the top card of the talon.
                 (dict-update-in `(,winner cards)               (curry cons winners-card))
                 ;; The loser picks up the next card.
                 (dict-update-in `(,(next-player winner) cards) (curry cons losers-card))
                 ;; The winner places the cards on their trick pile.
                 (dict-update-in `(,winner tricks) (curry append (list lead follow)))
                 ;; Drop two cards from the talon.
                 (dict-update-in '(talon cards) cddr)
                 ;; Winner of the trick leads again.
                 (dict-set* 'turn winner
                            'phase 'lead))))

(define (lead* state)
  (define player (dict-ref state 'turn))
  (cond
    ;; The player's hand is depleted. The game has come to an end.
    ((null? (dict-ref-in state `(,player cards)))
     (end state))
    ;; The player leads the trick.
    (else
     ;; What should the player lead?
     (define lead (pick-a-card state))
     ;; Flip the card up.
     (send table card-face-up lead)
     ;; Move the card to the player's trick region on the table.
     (send table move-cards-to-region (list lead) (dict-ref-in state `(,player trick-region)))
     (game-loop (~> state
                    ;; Remove the card led from the player's hand.
                    (dict-update-in `(,player cards) #λ(remove lead %))
                    ;; Put the card on the table.
                    (dict-set 'card-led lead)
                    ;; It's next player's turn to follow to the trick.
                    (dict-update 'turn next-player)
                    (dict-set 'phase 'follow*))))))

(define (follow* state)
  (define player (dict-ref state 'turn))
  (define lead (dict-ref state 'card-led))
  (define follow (pick-a-card state))
  ;; Who wins the trick?
  (define winner (if (beats? lead follow (dict-ref state 'trump))
                     (next-player player)
                     player))
  ;; Flip the played card up.
  (send table card-face-up follow)
  ;; Move the card to the player's trick region on the table.
  (send table move-cards-to-region (list follow) (dict-ref-in state `(,player trick-region)))
  (send table card-face-up follow)
  ;; Collect the trick.
  (sleep/yield 0.1)
  (send table move-cards-to-region (list lead follow) (dict-ref-in state `(,winner pile-region)))
  ;; Flip the cards over.
  (send table cards-face-down (list lead follow))
  (game-loop (~> state
                 (dict-set* 'phase 'lead*
                            'card-led #f)
                 ;; Remove the played card from hand.
                 (dict-update-in `(,player cards) #λ(remove follow %))
                 ;; The winner places the cards on their trick pile.
                 (dict-update-in `(,winner tricks) #λ(append (list lead follow) %))
                 ;; Winner of the trick leads again.
                 (dict-set 'turn winner))))

(define (end state)
  (send table set-status
        (apply format "Game over! Player pile: ~A cards | Opponent pile: ~A cards"
               (map (λ (player)
                      (length (dict-ref-in state `(,player tricks))))
                    '(player opponent))))
  (sleep/yield 10)
  (quit!))

(define (game-loop state)
  (send table set-status
        (format "It's ~A's turn. | Remaining cards: ~A"
                (dict-ref state 'turn)
                (length (dict-ref-in state '(talon cards)))))
  (sort-cards! state)
  (case (dict-ref state 'phase)
    ;; First stage
    ((lead)
     (lead state))
    ((follow)
     (follow state))
    ;; Second stage
    ((lead*)
     (lead* state))
    ((follow*)
     (follow* state))))


(module+ main
  (application-quit-handler quit!)
  (application-about-handler about)
  (error-display-handler
   (λ (str ex)
     (displayln str)
     (when (exn:fail? ex)
       (message-box "Error" str table '(stop ok)))))
  (let*-values (((player-cards opponent-cards talon-cards talon-cards*)
                 (apply values (split-into pack 13)))
                ((game-state)
                 {'phase 'lead
                  'turn (if (toss-coin?) 'player 'opponent)
                  'player {'cards player-cards             ; List of cards in player's hand
                           'tricks null                    ; List of cards taken by the player
                           'region player-region           ; Region that contains the player's hand
                           'trick-region player-field      ; Region that contains the card played by the player
                           'pile-region player-pile}       ; Region that contains the cards taken by the player
                  'opponent {'cards opponent-cards         ; List of cards in the opponent's hand
                             'tricks null                  ; List of cards taken by the opponent
                             'region opponent-region       ; Region that contains the opponent's hand
                             'trick-region opponent-field  ; Region that contains the card played by the opponent
                             'pile-region opponent-pile}   ; Region that contains the cards taken by the opponent
                  'card-led #f                             ; The card that was led to the trick
                  'talon {'cards (append talon-cards talon-cards*)  ; Cards in the talon
                          'region talon-region}                     ; The region that contains the talon
                  'trump (send (car talon-cards) get-suit)}))       ; The trump suit
    (prepare-table! game-state)
    (game-loop game-state)))
