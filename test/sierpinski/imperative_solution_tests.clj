(ns sierpinski.imperative-solution-tests
  (:require [clojure.test :refer :all]
            [sierpinski.imperative-solution :refer [print-fractal]]))

(deftest print-frac-iteration-0
  (binding [*out* (java.io.StringWriter.)]
    (print-fractal 0)
    (is (= (str *out*)
  "_______________________________1_______________________________\n______________________________111______________________________\n_____________________________11111_____________________________\n____________________________1111111____________________________\n___________________________111111111___________________________\n__________________________11111111111__________________________\n_________________________1111111111111_________________________\n________________________111111111111111________________________\n_______________________11111111111111111_______________________\n______________________1111111111111111111______________________\n_____________________111111111111111111111_____________________\n____________________11111111111111111111111____________________\n___________________1111111111111111111111111___________________\n__________________111111111111111111111111111__________________\n_________________11111111111111111111111111111_________________\n________________1111111111111111111111111111111________________\n_______________111111111111111111111111111111111_______________\n______________11111111111111111111111111111111111______________\n_____________1111111111111111111111111111111111111_____________\n____________111111111111111111111111111111111111111____________\n___________11111111111111111111111111111111111111111___________\n__________1111111111111111111111111111111111111111111__________\n_________111111111111111111111111111111111111111111111_________\n________11111111111111111111111111111111111111111111111________\n_______1111111111111111111111111111111111111111111111111_______\n______111111111111111111111111111111111111111111111111111______\n_____11111111111111111111111111111111111111111111111111111_____\n____1111111111111111111111111111111111111111111111111111111____\n___111111111111111111111111111111111111111111111111111111111___\n__11111111111111111111111111111111111111111111111111111111111__\n_1111111111111111111111111111111111111111111111111111111111111_\n111111111111111111111111111111111111111111111111111111111111111\n"))))

(deftest print-frac-iteration-1
  (binding [*out* (java.io.StringWriter.)]
    (print-fractal 1)
    (is (= (str *out*)
  "_______________________________1_______________________________\n______________________________111______________________________\n_____________________________11111_____________________________\n____________________________1111111____________________________\n___________________________111111111___________________________\n__________________________11111111111__________________________\n_________________________1111111111111_________________________\n________________________111111111111111________________________\n_______________________11111111111111111_______________________\n______________________1111111111111111111______________________\n_____________________111111111111111111111_____________________\n____________________11111111111111111111111____________________\n___________________1111111111111111111111111___________________\n__________________111111111111111111111111111__________________\n_________________11111111111111111111111111111_________________\n________________1111111111111111111111111111111________________\n_______________1_______________________________1_______________\n______________111_____________________________111______________\n_____________11111___________________________11111_____________\n____________1111111_________________________1111111____________\n___________111111111_______________________111111111___________\n__________11111111111_____________________11111111111__________\n_________1111111111111___________________1111111111111_________\n________111111111111111_________________111111111111111________\n_______11111111111111111_______________11111111111111111_______\n______1111111111111111111_____________1111111111111111111______\n_____111111111111111111111___________111111111111111111111_____\n____11111111111111111111111_________11111111111111111111111____\n___1111111111111111111111111_______1111111111111111111111111___\n__111111111111111111111111111_____111111111111111111111111111__\n_11111111111111111111111111111___11111111111111111111111111111_\n1111111111111111111111111111111_1111111111111111111111111111111\n"))))

(deftest print-frac-iteration-2
  (binding [*out* (java.io.StringWriter.)]
    (print-fractal 2)
    (is (= (str *out*)
  "_______________________________1_______________________________\n______________________________111______________________________\n_____________________________11111_____________________________\n____________________________1111111____________________________\n___________________________111111111___________________________\n__________________________11111111111__________________________\n_________________________1111111111111_________________________\n________________________111111111111111________________________\n_______________________1_______________1_______________________\n______________________111_____________111______________________\n_____________________11111___________11111_____________________\n____________________1111111_________1111111____________________\n___________________111111111_______111111111___________________\n__________________11111111111_____11111111111__________________\n_________________1111111111111___1111111111111_________________\n________________111111111111111_111111111111111________________\n_______________1_______________________________1_______________\n______________111_____________________________111______________\n_____________11111___________________________11111_____________\n____________1111111_________________________1111111____________\n___________111111111_______________________111111111___________\n__________11111111111_____________________11111111111__________\n_________1111111111111___________________1111111111111_________\n________111111111111111_________________111111111111111________\n_______1_______________1_______________1_______________1_______\n______111_____________111_____________111_____________111______\n_____11111___________11111___________11111___________11111_____\n____1111111_________1111111_________1111111_________1111111____\n___111111111_______111111111_______111111111_______111111111___\n__11111111111_____11111111111_____11111111111_____11111111111__\n_1111111111111___1111111111111___1111111111111___1111111111111_\n111111111111111_111111111111111_111111111111111_111111111111111\n"))))


(deftest print-frac-iteration-3
  (binding [*out* (java.io.StringWriter.)]
    (print-fractal 3)
    (is (= (str *out*)
  "_______________________________1_______________________________\n______________________________111______________________________\n_____________________________11111_____________________________\n____________________________1111111____________________________\n___________________________1_______1___________________________\n__________________________111_____111__________________________\n_________________________11111___11111_________________________\n________________________1111111_1111111________________________\n_______________________1_______________1_______________________\n______________________111_____________111______________________\n_____________________11111___________11111_____________________\n____________________1111111_________1111111____________________\n___________________1_______1_______1_______1___________________\n__________________111_____111_____111_____111__________________\n_________________11111___11111___11111___11111_________________\n________________1111111_1111111_1111111_1111111________________\n_______________1_______________________________1_______________\n______________111_____________________________111______________\n_____________11111___________________________11111_____________\n____________1111111_________________________1111111____________\n___________1_______1_______________________1_______1___________\n__________111_____111_____________________111_____111__________\n_________11111___11111___________________11111___11111_________\n________1111111_1111111_________________1111111_1111111________\n_______1_______________1_______________1_______________1_______\n______111_____________111_____________111_____________111______\n_____11111___________11111___________11111___________11111_____\n____1111111_________1111111_________1111111_________1111111____\n___1_______1_______1_______1_______1_______1_______1_______1___\n__111_____111_____111_____111_____111_____111_____111_____111__\n_11111___11111___11111___11111___11111___11111___11111___11111_\n1111111_1111111_1111111_1111111_1111111_1111111_1111111_1111111\n"))))


(deftest print-frac-iteration-4
  (binding [*out* (java.io.StringWriter.)]
    (print-fractal 4)
    (is (= (str *out*)
  "_______________________________1_______________________________\n______________________________111______________________________\n_____________________________1___1_____________________________\n____________________________111_111____________________________\n___________________________1_______1___________________________\n__________________________111_____111__________________________\n_________________________1___1___1___1_________________________\n________________________111_111_111_111________________________\n_______________________1_______________1_______________________\n______________________111_____________111______________________\n_____________________1___1___________1___1_____________________\n____________________111_111_________111_111____________________\n___________________1_______1_______1_______1___________________\n__________________111_____111_____111_____111__________________\n_________________1___1___1___1___1___1___1___1_________________\n________________111_111_111_111_111_111_111_111________________\n_______________1_______________________________1_______________\n______________111_____________________________111______________\n_____________1___1___________________________1___1_____________\n____________111_111_________________________111_111____________\n___________1_______1_______________________1_______1___________\n__________111_____111_____________________111_____111__________\n_________1___1___1___1___________________1___1___1___1_________\n________111_111_111_111_________________111_111_111_111________\n_______1_______________1_______________1_______________1_______\n______111_____________111_____________111_____________111______\n_____1___1___________1___1___________1___1___________1___1_____\n____111_111_________111_111_________111_111_________111_111____\n___1_______1_______1_______1_______1_______1_______1_______1___\n__111_____111_____111_____111_____111_____111_____111_____111__\n_1___1___1___1___1___1___1___1___1___1___1___1___1___1___1___1_\n111_111_111_111_111_111_111_111_111_111_111_111_111_111_111_111\n"))))


(deftest print-frac-iteration-5
  (binding [*out* (java.io.StringWriter.)]
    (print-fractal 5)
    (is (= (str *out*)
  "_______________________________1_______________________________\n______________________________1_1______________________________\n_____________________________1___1_____________________________\n____________________________1_1_1_1____________________________\n___________________________1_______1___________________________\n__________________________1_1_____1_1__________________________\n_________________________1___1___1___1_________________________\n________________________1_1_1_1_1_1_1_1________________________\n_______________________1_______________1_______________________\n______________________1_1_____________1_1______________________\n_____________________1___1___________1___1_____________________\n____________________1_1_1_1_________1_1_1_1____________________\n___________________1_______1_______1_______1___________________\n__________________1_1_____1_1_____1_1_____1_1__________________\n_________________1___1___1___1___1___1___1___1_________________\n________________1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1________________\n_______________1_______________________________1_______________\n______________1_1_____________________________1_1______________\n_____________1___1___________________________1___1_____________\n____________1_1_1_1_________________________1_1_1_1____________\n___________1_______1_______________________1_______1___________\n__________1_1_____1_1_____________________1_1_____1_1__________\n_________1___1___1___1___________________1___1___1___1_________\n________1_1_1_1_1_1_1_1_________________1_1_1_1_1_1_1_1________\n_______1_______________1_______________1_______________1_______\n______1_1_____________1_1_____________1_1_____________1_1______\n_____1___1___________1___1___________1___1___________1___1_____\n____1_1_1_1_________1_1_1_1_________1_1_1_1_________1_1_1_1____\n___1_______1_______1_______1_______1_______1_______1_______1___\n__1_1_____1_1_____1_1_____1_1_____1_1_____1_1_____1_1_____1_1__\n_1___1___1___1___1___1___1___1___1___1___1___1___1___1___1___1_\n1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1\n"))))