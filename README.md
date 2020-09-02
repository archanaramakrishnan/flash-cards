# Flashcards Program 

**Functional Programming & Domain Specific Languages (EECS 776) Final Project**
<br>
A program to review a deck of flashcards read in from file, implemented in Haskell. The program uses the Text.Layout.Table library to format the flash cards. It creates a 'deck' of cards by recursively building a list of the questions and answers. It loops through until the deck is empty, checking for the key presses using IO Unit. I created a frame instance of a shape using  newtype to render the flash card on the welcome page.
## How to use:

1. **Compile:** Go to the **flash-cards** directory and type `ghc --make flash_cards.hs`<br>
2. **Run:** Type **./flash_cards quiz.txt**, where quiz.txt can be replaced with any other text file holding review material. 
They must be formatted with each card in a new line, with the question and answer separated by a colon like "question goes here : and answer goes here"<br>
3. **Follow the instructions on the screen!**
<br><br>


## Sample Run
![Welcome](https://github.com/archanaramakrishnan/flash-cards/blob/master/screenshots/Welcome.png) <br>
![Answer](https://github.com/archanaramakrishnan/flash-cards/blob/master/screenshots/Answer.png) <br>
![Exit](https://github.com/archanaramakrishnan/flash-cards/blob/master/screenshots/Exit.png) <br>


These are the websites I used to create example text files:<br> https://www.science.co.il/elements/ <br> http://www.english-for-students.com/GRE-Word-List.html <br> https://www.kids-world-travel-guide.com/geography-trivia.html
