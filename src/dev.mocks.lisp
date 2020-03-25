(in-package #:dev.mocks)

(defparameter basic-cv '((:TITLE . "newline344") (:SUB-TITLE . "LOL")
                         (:IMAGE-DESCRIPTION . "ododododo")
                         (:CONTACT (:MAIL . "davd33@gmail.com")
                          (:LINKEDIN . "https://www.linkedin.com/in/davd33-rueda/")
                          (:GITHUB . "https://www.github.com/davd33"))
                         (:WORK-EXPERIENCES
                          ((:TITLE . "IT Consultant") (:COMPANY . "Technology Partner")
                           (:DESCRIPTION
                            . "Working in Luxembourg for the Banque Internationale du Luxembourg as a Java and React developer.")
                           (:DURATION . "2019 - 10mo")
                           (:TECHNOLOGIES "Spring boot" "Scala" "React.js"))
                          ((:TITLE . "Software Engineer") (:COMPANY . "CGI Darmstadt")
                           (:REF
                            . "https://drive.google.com/file/d/0B3Wu5re6rOM3aHV0THpPU1lrSGc/view?usp=sharing")
                           (:DESCRIPTION
                            . "Galileo - European Navigation System and ESA's satellite management software.")
                           (:DURATION . "2013-2017 - 3+y")
                           (:TECHNOLOGIES "Angular 4" "Node.js" "AWS")))
                         (:READINGS
                          ((:TITLE . "Practical Common Lisp")
                           (:IMAGE . "practical-common-lisp.cover.gif")
                           (:EXTERNAL-URL . "http://www.gigamonkeys.com/book/"))
                          ((:TITLE . "Professional Clojure")
                           (:IMAGE . "professional-clojure.cover.jpg")
                           (:EXTERNAL-URL
                            . "https://www.oreilly.com/library/view/professional-clojure/9781119267270/"))
                          ((:TITLE . "Clojure for the brave and true")
                           (:IMAGE . "clojure-for-the-brave-and-true.cover.jpg")
                           (:EXTERNAL-URL . "https://www.braveclojure.com/")))
                         (:PARAGRAPHS
                          ((:SECTION . "my-experience-with-lisp")
                           (:ELEMENTS
                            ((:PARAGRAPH . "1") (:ORDER . 1)
                             (:CONTENT
                              . "I have  been introduced to  functional programming end of  2018 as our  team was given the task to develop a new internal front-end application with React.js. We were coached by an external professional every two weeks."))
                            ((:PARAGRAPH . "2") (:ORDER . 2)
                             (:CONTENT
                              . "I have  been introduced to  functional programming end of  2018 as our  team was given the task to develop a new internal front-end application with React.js. We were coached by an external professional every two weeks."))
                            ((:PARAGRAPH . "2") (:ORDER . 3)
                             (:CONTENT (:LINK . "http://learnyouahaskell.com/")
                                       (:TEXT . "learn me a Haskell for a great good.")))
                            ((:PARAGRAPH . "3") (:ORDER . 4)
                             (:CONTENT
                              . "Later on, a colleague  of mine told me about clojure so  passionately that I let Haskell aside and started learning  Clojure! I learned and practiced Clojure, reagent,  re-frame, compojure, ring (I might forget some). I made a  presentation about it at my company ("))
                            ((:PARAGRAPH . "3") (:ORDER . 5)
                             (:CONTENT (:LINK . "https://github.com/davd33/cloj-tp")
                                       (:TEXT . "the slides")))
                            ((:PARAGRAPH . "3") (:ORDER . 6)
                             (:CONTENT
                              . " were a re-frame project  in order to demonstrate the fast feedback loop obtained by using the REPL with Figwheel). I went to the “heart  of clojure” in Leuven,  Belgium, met a lot  of interesting people there!"))
                            ((:PARAGRAPH . "4") (:ORDER . 7)
                             (:CONTENT
                              . "In order to deepen my understanding of Lisp, I started to learn Common-Lisp in August 2019. I created this web-page with the HTML generation library for CL: "))
                            ((:PARAGRAPH . "4") (:ORDER . 8)
                             (:CONTENT (:LINK . "https://github.com/ruricolist/spinneret")
                                       (:TEXT . "Spinneret")))
                            ((:PARAGRAPH . "4") (:ORDER . 9)
                             (:CONTENT
                              . ". It taught me a lot about how to manage with macros as well as how to use Quicklisp and much more."))))
                          ((:SECTION . "about.me.txt.p")
                           (:ELEMENTS
                            ((:PARAGRAPH . "1") (:ORDER . 1) (:CONTENT . "Free software is Love."))
                            ((:PARAGRAPH . "1") (:ORDER . 2)
                             (:CONTENT
                              . "For a better quality, for more security and for a better respect of human rights. It is the way to go if we want to have a chance to evolve our consciousness as a society and as individuals."))
                            ((:PARAGRAPH . "1") (:ORDER . 3)
                             (:CONTENT
                              . "I've been working in different areas. From computational biology in bio-image recognition to the space industry with the automation of the Galileo satellite fleet and the maintenance of ESA's SCOS-2000 and now in Finance, I've been through different aspects of developing IT products."))
                            ((:PARAGRAPH . "1") (:ORDER . 4)
                             (:CONTENT
                              . "Communication with transparency and benevolance is the absolute must for a healthy work collaboration."))
                            ((:PARAGRAPH . "1") (:ORDER . 5)
                             (:CONTENT . "And Lisp is the fatal weapon.")))))))
