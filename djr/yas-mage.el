;;; yas-mage.el --- Functions for yasnippet in a Magento context

;;; Commentary:
;;; Includes code for getting company and module names, as well as formatting
;;; getters and setters.

;;; Code:

(defclass yas-mage/field ()
  ((name :initarg :name
         :initform ""
         :type string
         :documentation "The name of the field.")
   (type :initarg :type
         :initform "mixed"
         :type string
         :documentation "The type of the field.")
   (visibility :initarg :visibility
               :initform 'private
               :type symbol
               :documentation "The visibility of the field.")
   (container :initarg :container
              :initform 'class
              :type symbol
              :documentation "The container the field is in, either 'class or 'interface."))
  "A Magento class or interface field.")

(cl-defmethod yas-mage/field-format-declaration ((field yas-mage/field))
  "Format the declaration of FIELD depending on visibility."
  (with-slots (name type visibility) field
    (if (eq visibility 'const)
        (format "const %s = %S;" (upcase name) name)
      (format "\
/**
 * %s
 *
 * @var %s
 */
%s %s;
"
              (yas-mage/snake-case-to-words name)
              type
              visibility
              name))))

(cl-defmethod yas-mage/field-format-getter ((field yas-mage/field)
                                            &optional body)
  "Format getter for FIELD."
  (with-slots (name type container) field
    (format
     "\
/**
 * Get %s
 *
 * @return %s
 */
public function get%s()%s"
     (yas-mage/snake-case-to-words name)
     type
     (string-inflection-upper-camelcase-function name)
     (yas-mage/format-method-body container body))))

(cl-defmethod yas-mage/field-format-setter ((field yas-mage/field)
                                            parent-type
                                            &optional body)
  "Format setter for FIELD."
  (with-slots (name type container) field
    (format
     "\
/**
 * Set %s
 *
 * @param %s $%s
 *
 * @return %s
 */
public function set%s($%s)%s"
     (yas-mage/snake-case-to-words name)
     type
     (string-inflection-camelcase-function name)
     parent-type
     (string-inflection-upper-camelcase-function name)
     (string-inflection-camelcase-function name)
     (yas-mage/format-method-body container body))))

(defun yas-mage/format-method-body (class-or-interface &optional body)
  "Formats a method body BODY based on CLASS-OR-INTERFACE.
If CLASS-OR-INTERFACE is 'class, return BODY encased in braces.
If CLASS-OR-INTERFACE is 'interface, return ';'"
  (cond ((eq class-or-interface 'class) (format "\n{\n%s\n}" (if body body "")))
        ((eq class-or-interface 'interface) ";")))

(defun yas-mage/snake-case-to-words (str)
  "Format a snake-cased string STR as upcased words."
  (upcase-initials (replace-regexp-in-string "_" " " str)))

(defun yas-mage/company-name ()
  "When visiting a Magento module under app/code, get the company name."
  (let ((dir (djr/buffer-file-directory)))
    (if (and dir (string-match "app/code/\\([^/]+\\)/" dir))
        (match-string-no-properties 1 dir)
      "")))

(defun yas-mage/module-name ()
  "When visiting a Magento module under app/code, get the module name."
  (let ((dir (djr/buffer-file-directory)))
    (if (and dir (string-match "app/code/[^/]+/\\([^/]+\\)" dir))
        (match-string-no-properties 1 dir)
      "")))

(defun yas-mage/module-code ()
  "When visiting a Magento module under app/code: get the COMPANY_MODULE formatted module code."
  (let ((dir (djr/buffer-file-directory)))
    (if (and dir (string-match "app/code/\\([^/]+\\)/\\([^/]+\\)" dir))
        (format "%s_%s"
                (match-string-no-properties 1 dir)
                (match-string-no-properties 2 dir))
      "")))

(defun yas-mage/module-namespace ()
  "When visiting a Magento module under app/code: get the COMPANY\\MODULE formated module namespace."
  (replace-regexp-in-string "_" "\\" (yas-mage/module-code) t t))

(defun yas-mage/namespace ()
  "When visiting a Magento module under app/code, get the namespace of that file."
  (let ((dir (djr/buffer-file-directory)))
    (if (and dir (string-match "app/code/\\(.+\\)/" dir))
        (replace-regexp-in-string "/" "\\" (match-string-no-properties 1 dir) t t)
      "")))

(defun yas-mage/integration-test-p ()
  "Returns true if in an integration test."
  (let ((dir (djr/buffer-file-directory)))
    (and dir (string-match "Test/Integration" dir))))

(defun yas-mage/integration-test-namespace ()
  "When visiting an integration Test file in a Magento module under app/code, get the namespace of the Test without the Test/Integration part of the path."
  (let ((namespace (yas-mage/namespace)))
    (replace-regexp-in-string "\\\\Test\\\\Integration" "" namespace)))

(defun yas-mage/last-prompt-field-list ()
  "Return the last built list of fields."
  djr/last-prompt-built-list)

(defun yas-mage/prompt-field-list (visibility &optional container)
  "Prompt for a list of fields with VISIBILITY in CONTAINER."
  (setq djr/last-prompt-built-list
        (seq-map (lambda (field)
                   (with-slots ((field-visibility visibility) (field-container container)) field
                     (setf field-visibility visibility)
                     (setf field-container container)
                     field))
                 (yas-mage/last-prompt-field-list)))
  (djr/prompt-build-list
   "Enter fields (TYPE NAME, NAME alone will result in type 'mixed')"
   (lambda (item)
     (let* ((type-name (split-string item))
            (type (if (> (length type-name) 1) (pop type-name) "mixed"))
            (name (pop type-name)))
       (yas-mage/field
        :name name
        :type type
        :visibility visibility
        :container container)))
   (lambda (item)
     (with-slots (type name) item
       (format "%s %s" type name)))))

(provide 'yas-mage)
;;; yas-mage.el ends here
