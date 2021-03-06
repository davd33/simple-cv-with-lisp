(in-package #:dev.mocks)

(defparameter basic-cv "{
  \"title\": \"'( :david :rueda )\",
  \"sub-title\": \"Open your Mind and Technologies\",
  \"image-description\": \"me in an image\",
  \"contact\": {
    \"mail\": \"davd33@gmail.com\",
    \"linkedin\": \"https://www.linkedin.com/in/davd33-rueda/\",
    \"github\": \"https://www.github.com/davd33\"
  },
  \"work-experiences\": [
    {
      \"title\": \"IT Consultant\",
      \"company\": \"Technology Partner\",
      \"description\": \"Working in Luxembourg for the Banque Internationale du Luxembourg as a Java and React developer.\",
      \"duration\": \"2019 - 10mo\",
      \"technologies\": [
        \"Spring boot\",
        \"Scala\",
        \"React.js\"
      ]
    },
    {
      \"title\": \"Entrepreneur\",
      \"description\": \"Development of an App for training German. A jump outside the comfort zone. Introduction to micro-services.\",
      \"duration\": \"2017 - 6mo\",
      \"technologies\": [
        \"Angular 4\",
        \"Node.js\",
        \"AWS\",
        \"Seneca.js\"
      ]
    },
    {
      \"title\": \"Software Engineer\",
      \"company\": \"CGI Darmstadt\",
      \"ref\": \"https://drive.google.com/file/d/0B3Wu5re6rOM3aHV0THpPU1lrSGc/view?usp=sharing\",
      \"description\": \"Galileo - European Navigation System and ESA's satellite management software.\",
      \"duration\": \"2013-2017 - 3+y\",
      \"technologies\": [
        \"Angular 4\",
        \"Node.js\",
        \"AWS\"
      ]
    },
    {
      \"title\": \"Software Engineer\",
      \"company\": \"BIOTEC\",
      \"description\": \"Drug discovery for bone diseases: towards high-throughput and high-content phenotype comparison.\",
      \"duration\": \"2012-2013 - 9mo\",
      \"technologies\": [
        \"Java\",
        \"R\",
        \"Swing\",
        \"Weka ML\",
        \"ImageJ\"
      ]
    },
    {
      \"title\": \"Team Leader\",
      \"company\": \"Amerbank\",
      \"remote\": true,
      \"description\": \"Leading a development team for the creation of an internal AML facilities. A micro-service approach.\",
      \"duration\": \"2018 - 9mo\",
      \"technologies\": [
        \"Istio\",
        \"Kubernetes\",
        \"AWS\",
        \"Circle CI\"
      ]
    }
  ],
  \"readings\": [
    {
      \"title\": \"Practical Common Lisp\",
      \"image\": \"practical-common-lisp.cover.gif\",
      \"external-url\": \"http://www.gigamonkeys.com/book/\"
    },
    {
      \"title\": \"Professional Clojure\",
      \"image\": \"professional-clojure.cover.jpg\",
      \"external-url\": \"https://www.oreilly.com/library/view/professional-clojure/9781119267270/\"
    },
    {
      \"title\": \"Clojure for the brave and true\",
      \"image\": \"clojure-for-the-brave-and-true.cover.jpg\",
      \"external-url\": \"https://www.braveclojure.com/\"
    }
  ],
  \"paragraphs\": [
    {
      \"section\": \"my-experience-with-lisp\",
      \"elements\": [
        {
          \"paragraph\": \"1\",
          \"order\": 1,
          \"content\": \"I have  been introduced to  functional programming end of  2018 as our  team was given the task to develop a new internal front-end application with React.js. We were coached by an external professional every two weeks.\"
        },
        {
          \"paragraph\": \"2\",
          \"order\": 2,
          \"content\": \"I have  been introduced to  functional programming end of  2018 as our  team was given the task to develop a new internal front-end application with React.js. We were coached by an external professional every two weeks.\"
        },
        {
          \"paragraph\": \"2\",
          \"order\": 3,
          \"content\": {
            \"link\": \"http://learnyouahaskell.com/\",
            \"text\": \"learn me a Haskell for a great good.\"
          }
        },
        {
          \"paragraph\": \"3\",
          \"order\": 4,
          \"content\": \"Later on, a colleague  of mine told me about clojure so  passionately that I let Haskell aside and started learning  Clojure! I learned and practiced Clojure, reagent,  re-frame, compojure, ring (I might forget some). I made a  presentation about it at my company (\"
        },
        {
          \"paragraph\": \"3\",
          \"order\": 5,
          \"content\": {
            \"link\": \"https://github.com/davd33/cloj-tp\",
            \"text\": \"the slides\"
          }
        },
        {
          \"paragraph\": \"3\",
          \"order\": 6,
          \"content\": \" were a re-frame project  in order to demonstrate the fast feedback loop obtained by using the REPL with Figwheel). I went to the “heart  of clojure” in Leuven,  Belgium, met a lot  of interesting people there!\"
        },
        {
          \"paragraph\": \"4\",
          \"order\": 7,
          \"content\": \"In order to deepen my understanding of Lisp, I started to learn Common-Lisp in August 2019. I created this web-page with the HTML generation library for CL: \"
        },
        {
          \"paragraph\": \"4\",
          \"order\": 8,
          \"content\": {
            \"link\": \"https://github.com/ruricolist/spinneret\",
            \"text\": \"Spinneret\"
          }
        },
        {
          \"paragraph\": \"4\",
          \"order\": 9,
          \"content\": \". It taught me a lot about how to manage with macros as well as how to use Quicklisp and much more.\"
        }
      ]
    },
    {
      \"section\": \"about.me.txt.p\",
      \"elements\": [
        {
          \"paragraph\": \"1\",
          \"order\": 1,
          \"content\": \"Free software is Love.\"
        },
        {
          \"paragraph\": \"2\",
          \"order\": 2,
          \"content\": \"For a better quality, for more security and for a better respect of human rights. It is the way to go if we want to have a chance to evolve our consciousness as a society and as individuals.\"
        },
        {
          \"paragraph\": \"3\",
          \"order\": 3,
          \"content\": \"I've been working in different areas. From computational biology in bio-image recognition to the space industry with the automation of the Galileo satellite fleet and the maintenance of ESA's SCOS-2000 and now in Finance, I've been through different aspects of developing IT products.\"
        },
        {
          \"paragraph\": \"4\",
          \"order\": 4,
          \"content\": \"Communication with transparency and benevolance is the absolute must for a healthy work collaboration.\"
        },
        {
          \"paragraph\": \"5\",
          \"order\": 5,
          \"content\": \"And Lisp is the fatal weapon.\"
        }
      ]
    }
  ]
}")

(defparameter local-api-url "http://localhost:5000")

(defun api-cv ()
  "Makes an http request to /cv.
Requires that the server be running."
  (dex:post (str:concat local-api-url "/cv")
            :content basic-cv))

(defun home ()
  "Makes an http request to /home"
  (dex:get (str:concat local-api-url "/home")))
