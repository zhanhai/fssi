package fssi.types

/** Transaction Result
  * @param success the transaction has run through without any error.
  * @param costs the total cost of 
  */
case class TransactionResult(
  success: Boolean,
  totalCosts: Int,
  logs: Vector[String],
  sqlStoreHash: Hash,
  kvStoreHash: Hash
)

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))
