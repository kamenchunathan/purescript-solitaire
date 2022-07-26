# TODO:

1. Derive show for the types in the game  [x]
2. create functions for:
    - creating a random deck / shuffling a deck  []
    - creating the 4 types of piles from a deck : stock, tableau, waste (initially empty), and foundations [x]
    - converting a card to a path with the uri for the card asset [x]
3. Display the cards in the browser [x]
4. Set only top cards to be face in the beginning in the tableau and cards that have been flipped by user - might need a visibility flag on cards []
5. Find a better solution for styling []

    Some amount of technical debt in css has accumulated in css handling - and I am uncomfortable with cssa - a framework might help
6. Display code refactoring []

    some of the rendering  code is getting unweildy and may be difficult for me to understand in future. Heavily comment and split into smaller functions
7. Actual functionality and responsiveness
    - Add ability to move cards from stock to wasee by clicking waste []
    - drag and drop functionality - high priority []
    - rules for moving cards []