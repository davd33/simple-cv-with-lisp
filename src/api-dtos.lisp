(in-package #:api-dtos)

(defclass contact-dto ()
  ((mail :initarg :mail
         :accessor mail
         :documentation "Mail address.")
   (linkedin :initarg :linkedin
             :accessor linkedin
             :documentation "Linkedin URL.")
   (github :initarg :github
           :accessor github
           :documentation "Github link")))

(defclass work-experience-dto ()
  ((title :initarg :title
          :accessor title
          :documentation "Title of the work experience.")
   (company :initarg :company
            :accessor company
            :documentation "Company for which you worked.")
   (description :initarg :description
                :accessor description
                :documentation "Description of the work done.")
   (duration :initarg :duration
             :accessor duration
             :documentation "Duration of the experience.")
   (technologies :initarg :technologies
                 :accessor technologies
                 :documentation "Which technologies you've worked with.")))

(defclass reading-dto ()
  ((title :initarg :title
          :accessor title
          :documentation "Title of the book read.")
   (image :initarg :image
          :accessor image
          :documentation "Name of the image file associated with the book.")
   (external-url :initarg :external-url
                 :accessor external-url
                 :documentation "Where to find/buy the book.")))

(defclass paragraph-element-dto ()
  ((order :initarg :order
          :accessor order
          :documentation "Order of this element within the paragraph.")
   (content :initarg :content
            :accessor content
            :documentation "Contents can be a string or an alist (JSON object).")))

(defclass paragraph-dto ()
  ((elements :initarg :elements
             :accessor elements
             :documentation "List of paragraph elements that compose this paragraph.")))

(defclass section-dto ()
  ((title :initarg :title
          :accessor title
          :documentation "Title of the section.")
   (paragraphs :initarg :paragraphs
               :accessor paragraphs
               :documentation "The list of paragraphs for the section.")))

(defclass cv-dto ()
  ((title :initarg :title
          :initform (error "Must supply a title.")
          :accessor title
          :documentation "Title of the CV.")
   (sub-title :initarg :subtitle
              :accessor sub-title
              :documentation "Sub-title of the CV.")
   (image-description :initarg :image-description
                      :accessor image-description
                      :documentation "Image description for the CV's avatar.")
   (contact :initarg :contact
            :accessor contact
            :documentation "Contact information.")
   (work-experiences :initarg :work-experiences
                     :accessor work-experiences
                     :documentation "Details of every work experience.")
   (readings :initarg :readings
             :accessor readings
             :documentation "Books / content read and of interest.")
   (paragraphs :initarg :paragraphs
               :accessor paragraphs
               :documentation "Text sections of the CV.")))
