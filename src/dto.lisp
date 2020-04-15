(in-package #:dto)

(defclass contact-dto ()
  ((mail :initarg :mail
         :accessor mail
         :type string
         :documentation "Mail address.")
   (linkedin :initarg :linkedin
             :accessor linkedin
             :type string
             :documentation "Linkedin URL.")
   (github :initarg :github
           :accessor github
           :type string
           :documentation "Github link")))

(defclass work-experience-dto ()
  ((title :initarg :title
          :accessor title
          :type string
          :documentation "Title of the work experience.")
   (company-o :initarg :company-o
              :accessor company-o
              :type string
              :documentation "Company for which you worked.")
   (description :initarg :description
                :accessor description
                :type string
                :documentation "Description of the work done.")
   (ref-o :initarg :ref-o
          :accessor ref-o
          :type string
          :documentation "Reference from the company.")
   (remote-o :initarg :remote-o
             :accessor remote-o
             :type string
             :documentation "Remote (true/false) work flag.")
   (duration :initarg :duration
             :accessor duration
             :type string
             :documentation "Duration of the experience.")
   (technologies :initarg :technologies
                 :accessor technologies
                 :type string
                 :documentation "Which technologies you've worked with. String of Comma separated values.")))

(defclass reading-dto ()
  ((title :initarg :title
          :accessor title
          :type string
          :documentation "Title of the book read.")
   (image :initarg :image
          :accessor image
          :type string
          :documentation "Name of the image file associated with the book.")
   (external-url :initarg :external-url
                 :accessor external-url
                 :documentation "Where to find/buy the book.")))

(defclass paragraph-element-dto ()
  ((order :initarg :order
          :accessor order
          :type integer
          :documentation "Order of this element within the paragraph.")
   (content :initarg :content
            :accessor content
            :type string
            :documentation "Contents can be a string or an alist (JSON object).")))

(defclass paragraph-dto ()
  ((title :initarg :title
          :accessor title
          :type string
          :documentation "Title for the paragraph.")
   (element-list :initarg :element-list
                 :accessor element-list
                 :type paragraph-element-dto
                 :documentation "List of paragraph elements that compose this paragraph.")))

(defclass section-dto ()
  ((title :initarg :title
          :accessor title
          :type string
          :documentation "Title of the section.")
   (paragraph-list :initarg :paragraph-list
                   :accessor paragraph-list
                   :type paragraph-dto
                   :documentation "The list of paragraphs for the section.")))

(defclass cv-dto ()
  ((title :initarg :title
          :accessor title
          :type string
          :documentation "Title of the CV.")
   (sub-title :initarg :subtitle
              :accessor sub-title
              :type string
              :documentation "Sub-title of the CV.")
   (image-description :initarg :image-description
                      :accessor image-description
                      :type string
                      :documentation "Image description for the CV's avatar.")
   (contact :initarg :contact
            :accessor contact
            :type contact-dto
            :documentation "Contact information.")
   (work-experience-list :initarg :work-experience-list
                         :accessor work-experience-list
                         :type work-experience-dto
                         :documentation "Details of every work experience.")
   (reading-list :initarg :reading-list
                 :accessor reading-list
                 :type reading-dto
                 :documentation "Books / content read and of interest.")
   (section-list :initarg :section-list
                 :accessor section-list
                 :type section-dto
                 :documentation "Text sections of the CV.")))
