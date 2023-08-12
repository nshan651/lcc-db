(ql:quickload :drakma)
(ql:quickload :cl-csv)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-json)
(ql:quickload :babel)

(defparameter *api-url-literal* "https://www.loc.gov/{endpoint}/?fo=json"
  "Defines the api-url for LOC.")

(defparameter *csv-file* #P"../library_export.csv"
  "Define the file parameter. (Note that '#P' is a special pathname symbol).")

(defparameter *books* (cl-csv:read-csv *csv-file*)
  "Data frame of books.")

(defun non-zero (lst)
  "Removes all zeros from a list of items."
  (remove "0" lst :test #'string=))
 
(defun get-col-index (header col-name)
  "Gets the column index of a column name."
  (position col-name header :test #'string=))

(defun get-col (tbl col-name)
  "Extracts a column from a csv based on a target header."
  (let* ((header (first tbl))
	 (target-index (get-col-index header col-name))
	 (column (mapcar (lambda (row) (nth target-index row)) (rest tbl))))
    column))

(defun extract-numeric (string)
  "Parses a 10-digit or 13-digit integer from a string."
  (let ((matches (cl-ppcre:all-matches-as-strings "\\b[0-9]{10}\\b|\\b[0-9]{13}\\b" string)))
    (when matches
      (parse-integer (subseq (first matches) 1)))))

(defun parse-isbn (col)
  "Parses the ISBN-10 or ISBN-13 code from a list of strings."
  (remove-if-not #'identity (mapcar #'extract-numeric col)))
  
;; Selects column and dynamically find index based on column name.
(let ((column (get-col *books* "ISBN13")))
  (parse-isbn column))

;;; === Querying LOC ===

(defun output-stream (path)
  "Opens file for writing."
  (open path :direction :output :if-exists :supersede))

(defun write-data (data path)
  "Writes data to file."
  (let ((outstream (output-stream path)))
    (format outstream "~a" data)
    (close outstream)))

(defun decode-utf8 (bytes)
  "Decodes a series of utf8 bytes to a string."
  (let* ((byte-array
	   (make-array (length bytes) :element-type '(unsigned-byte 8)))
         (string-length (length bytes)))
    (loop for i from 0 below string-length do
          (setf (aref byte-array i) (aref bytes i)))
    (babel:octets-to-string byte-array :encoding :utf-8)))
   
;; (defvar test-url "http://www.loc.gov/item/2014717546/?fo=json")
(defvar test-url "https://www.loc.gov/search/?fo=json&q=0345376595")

;; "https://www.loc.gov/item/0140443487/?fo=json"
;; https://openlibrary.org/api/books?bibkeys=ISBN:0345376595&jscmd=data&format=json

(let ((payload
	(decode-utf8 (drakma:http-request test-url))))
  (write-data payload "../payload.json"))
