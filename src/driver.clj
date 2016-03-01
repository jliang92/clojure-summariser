(ns driver

  (:require [opennlp.nlp :as nlp])
  (:require [clojure.string :as str])
  )

;;used for spliting string into sentences.
(def get-sentences (nlp/make-sentence-detector "models/en-sent.bin"))


(def punctuations [" '", "' ", ".", "?", "!", "$", "(", ")", ",", ";"])

;;stop words are common words used in sentences
(def stop_words ["a", "and", "the", "me", "I", "of", "if", "it", "is", "they", "there", "but", "or", "to", "this", "you", "in", "your", "on", "for", "as", "are", "that", "with", "have", "be", "at", "or", "was", "so", "not", "an"])


;;replaces useless punctuation marks with whitespace
(defn remove-punctuation-marks
  [content]
  (reduce
    (fn
      [sentence word]
      (str/replace sentence word " ")
      )
    content
    punctuations
    )
  )

;;tokenizes the words in the content, generally used after removing punctuations
(defn get-words
  [content]
  (str/split (remove-punctuation-marks (str/lower-case content)) #" ")
  )

;;iteratively print contents of a list
(defn print-list
  [list]
  ;;(println "printing...")
  (def size (count list))
  (def index 0)
  (while (< index size)
    (println index ") " (nth list index))
    (def index (inc index))
    )
  )

;;prints the key and values of a map
(defn print-map-key-values
  [map]
  (println "printing map ...\n key -> value")

  (reduce
    (fn [v k] (println  k" -> " (get map k))) 0 (keys map))

  )

;;takes list of not unique words, finds the unique words by creating hashmap
;;uses the unique hashmap to iterate the not unique word-list to find the count
(defn get-word-frequency
  [word-list]

  ;;populating hashmap
  (def unique-map (sorted-map)) ;; the map of unique words in the text
  (loop [x 0]
    (when (< x (count word-list))
      (def word (str/trim (nth word-list x)))
      (if (< 0 (count word))
        (def unique-map (assoc unique-map word 0))
        )
      (recur (+ x 1))
      )
    )


  ;;removing stop words
  (loop [x 0]
    (when (< x (count stop_words))
      (def unique-map (dissoc unique-map (str/capitalize (nth stop_words x))))
      (def unique-map (dissoc unique-map (str/lower-case (nth stop_words x))))
      (recur (inc x))
      )
    )
  (if (contains? unique-map " ")
    (def unique-map (dissoc unique-map ("")))
    )

  ;;(println "After removing stop words")
  ;;(print-map-key-values unique-map)

  (loop [x 0]
    (when (< x (count word-list))

      (let [word (str/trim (nth word-list x))]

      ;;inc only if contains therefore must be true
      (if (contains? unique-map word)
        (def unique-map (update-in unique-map [word] inc))
	     )
		)
      (recur (+ x 1))
      )
    )

  unique-map
  )
;;summarizes content into num_sentences sentences
(defn summarize
  [content num_sentences]

  (def testSentences (get-sentences (str/lower-case content)))
  (def actualSentences (get-sentences content))
  (def outputSentences (sorted-map))

  ;;list of non-unique words from content
  (def words (get-words (str/lower-case content)))

  ;;map of unique words and their number of occurences  
  (def freq (get-word-frequency words))                     

  ;;number of sentences used for processing
  (def ws_size (count testSentences))
  (println  " # of sentences in content : "ws_size  "\n")

 (def index 1)

  ;;loop ends when there are enough sentences or when the collection of unique words (freq) is empty
  (while (and (not (empty? freq)) (< (count outputSentences) num_sentences))

    (let [word (key (apply max-key val freq))]
    (println "sentence count is " (count outputSentences)" ,word is " word " and value is " (get freq word))
    (def y 0)

    (while (and (< y ws_size) (> num_sentences (count outputSentences)))

       (let [test_sentence (nth testSentences y)
        sentence_words (set (get-words test_sentence))
	     actual_sentence (nth actualSentences y)]

         (when (contains? sentence_words word)
           (def outputSentences (assoc outputSentences (str actual_sentence)  index))
           (def index (inc index))
           (def y (inc ws_size))
	        ))
    (def y (inc y))
    )
   (def freq (dissoc freq word))
   )
  )


;;(println "Summary map is : ")
;;(print-map-key-values outputSentences)
  (println "\nnumber of outputSentences : " (count outputSentences)"\nnumber of request : " num_sentences)
  (if (= num_sentences (count outputSentences))
    (println "The number of output sentences are correct.")
    (println "The number of output sentences are incorrect.")
    )


(def summary (str ""))
(loop [d 0]
    (when (< d (count actualSentences))
		(let [sentence (nth actualSentences d)]
			(when (contains? outputSentences sentence)
				(def summary (str summary "" sentence "\n"))
			)
		)
    (recur (inc d))
    )
)


(println "Finished summarizing\n")
(println summary)
)




;;The way this summariser works is it finds the frequency of each word.
;;Then it picks sentences that have those words, which is why it was necessary to have a list of commonly used words AKA stop words.
;;It puts the selected sentences back in order based on the original user input string.
;;sample_test_main takes the number of sentences you want in your summary and the input string as arguments

(defn -main
  [ num_sentences content ]
  ;;(def content (str "These are just a few examples of scientific illiteracy inane misconceptions that could have been avoided with a smidgen of freshman science. (For those afraid to ask: pencil 'lead' is carbon; hydrogen fuel takes more energy to produce than it releases; all living things contain genes; a clone is just a twin.) Though we live in an era of stunning scientific understanding, all too often the average educated person will have none of it. People who would sneer at the vulgarian who has never read Virginia Woolf will insouciantly boast of their ignorance of basic physics. Most of our intellectual magazines discuss science only when it bears on their political concerns or when they can portray science as just another political arena. As the nation's math departments and biotech labs fill up with foreign students, the brightest young Americans learn better ways to sue one another or to capitalize on currency fluctuations. All this is on top of our nation's endless supply of New Age nostrums, psychic hot lines, creationist textbook stickers and other flimflam."))
  ;;(println content)
  (def limit  (count(get-sentences content)))

  (if  (and (> num_sentences 0)(< num_sentences limit))
	(summarize content num_sentences)
	(println "your choice of how many sentences must be within the range of  1 to "limit" for your given input string.")
  )
)
