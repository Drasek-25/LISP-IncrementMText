; Written by Patrick Marschlowitz 4/25/2025
; This function asks the user to select a set of objects
; it then takes all mtext in the drawing, finds the trailing numbers
; then adds 1 to that number. Currently only increments by 1 and the number
; must be at the end of the string.

; v2 Updated 8/14/25
; lisp now prompts user for increment direction and amount
; values default select to +1. Still only affects num at end of string
(defun c:Numchange () 
  (vl-load-com)
  ; Prompt object selection, filtering for MTEXT
  (setq selEnt (ssget '((0 . "MTEXT"))))

  ;reset values on restart of lisp
  (setq incrementDirection nil)
  (setq incrementAmount nil)

  ;if user selects a direction make it lowercase, else set it to "p"
  (setq incrementDirection (getstring "\nSelect [P]lus or [M]inus for incrementation, if not selected choice is [P]lus:\n"))
  ;for some reason getstring returns "" instead of nil when nothing is selected
  (if (= incrementDirection "") 
    (setq incrementDirection "p")
    (princ)
  )
  (setq incrementDirection (strcase incrementDirection T))


  ;if user selects an amount then set it, otherwise its set to 1
  (setq incrementAmount (getint "\nSpecify amount to increment, if not selected choice is [1]:\n"))
  (if (= incrementAmount nil) (setq incrementAmount 1) (princ))

  ; verify items were selected
  (if (and selEnt (> (sslength selEnt) 0)) 
    (progn 
      (setq i 0)

      ; iterate list of MTEXT
      (while (< i (sslength selEnt)) 
        (progn 
          ; Get the VLA object
          (setq obj (vlax-ename->vla-object (ssname selEnt i)))
          (setq old_text (vla-get-textstring obj))

          ; Convert string to list of characters
          (setq char-list (vl-string->list old_text))

          ; Reverse the list
          (setq reversed (reverse char-list))

          ;; Keep only digit characters (ASCII 48–57)
          (setq digits-only (vl-remove-if-not 
                              (function (lambda (c) (and (>= c 48) (<= c 57))))
                              reversed
                            )
          )
          ; Reverse and convert to int
          (setq trailing_num (atoi (vl-list->string (reverse digits-only))))

          ; set to empty string incase its not used
          (setq precedeing_text "")

          ; Verify that a preceding text exists
          (if (not (= (strlen old_text) (strlen (itoa trailing_num)))) 
            ; subtract the whole text from the numbers to subtr the text infront of mtext
            (setq precedeing_text (substr old_text 
                                          1
                                          (- (strlen old_text) 
                                             (strlen (itoa trailing_num))
                                          )
                                  )
            )
          )

          (progn 
            (if (= incrementDirection "p") 
              ;"THEN" - uptick number for "p"
              (setq new_num (+ trailing_num incrementAmount))
              ;"ELSE" - downtick number for "m"
              (setq new_num (- trailing_num incrementAmount))
            )
            ;push new number back into Mtext
            (setq new_text (strcat precedeing_text (itoa new_num)))
            (vla-put-textstring obj new_text)
          )

          (setq i (1+ i))
        )
      )
    )
    (princ "\nNo MText object found.")
  )
  (princ)
)
