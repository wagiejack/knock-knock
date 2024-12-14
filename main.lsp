(defstruct dns-header
    (id 0 :type (unsigned-byte 16))
    (flags 0 :type (unsigned-byte 16))
    (questions-count 0 :type (unsigned-byte 16))
    (answers-count 0 :type (unsigned-byte 16))
    (authority-count 0 :type (unsigned-byte 16))
    (additional-count 0 :type (unsigned-byte 16))
)

(defun make-dns-query-header ()
    (let ((header (make-dns-header) ))
        (setf (dns-header-id header) (+ 1 (random 65535)))
        (setf (dns-header-flags header) #x0100)
        (setf (dns-header-questions-count header) 1) 
    header))

(defun header-to-bytes(header)
    (let ((bytes (make-array 12 :element-type '(unsigned-byte 8))))
        ; id
        (setf (aref bytes 0) (ash (dns-header-id header) -8))
        (setf (aref bytes 1) (logand (dns-header-id header) #xFF))
        ; flags
        (setf (aref bytes 2) (ash (dns-header-flags header) -8))
        (setf (aref bytes 3) (logand (dns-header-flags header) #xFF))
        ; questions-count
        (setf (aref bytes 4) (ash (dns-header-questions-count header) -8))
        (setf (aref bytes 5) (logand (dns-header-questions-count header) #xFF))
        ; answers-count
        (setf (aref bytes 6) (ash (dns-header-answers-count header) -8))
        (setf (aref bytes 7) (logand (dns-header-answers-count header) #xFF))
        ; authority-count
        (setf (aref bytes 8) (ash (dns-header-authority-count header) -8))
        (setf (aref bytes 9) (logand (dns-header-authority-count header) #xFF))
        ; additional-count
        (setf (aref bytes 10) (ash (dns-header-additional-count header) -8))
        (setf (aref bytes 11) (logand (dns-header-additional-count header) #xFF))
        bytes
    )
)
