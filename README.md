# Kadi
> A card and board games site

A Site where you can play card games to be used for testing strategies and AI against humans.
Written in purescript with the halogen framework. 

The only game you can play currently is solitaire.

## Solitaire
First game I'm working on currently

### Roadmap
1. [x] Derive show for the types in the game 
2. [ ] create functions for:
    - [ ] creating a random deck / shuffling a deck  
    - [x] creating the 4 types of piles from a deck : stock, tableau, waste (initially empty), and foundations 
    - [x] converting a card to a path with the uri for the card asset 
3. [x] Display the cards in the browser 
4. [x] Set only top cards to be face in the beginning in the tableau and cards that have been flipped by user - might need a visibility flag on cards 
5. [x] Find a better solution for styling  - tailwind
   [x] Some amount of technical debt in css has accumulated in css handling - and I am uncomfortable with cssa - a framework might help
6. [x] Display code refactoring 
   [ ]  some of the rendering  code is getting unweildy and may be difficult for me to understand in future. Heavily comment and split into smaller functions
7. [ ] Actual functionality and responsiveness
    - [x] Add an ability to move cards from stock to waste by clicking waste 
    - [ ] rules for moving cards 
8. [ ] Drag and drop functionality - high priority 
9. [x] Add CI/CD and host application somewhere -very high priority 
10. [x] SPA features
    - [x] Landing page
    - [x] Routing

### Notes on Implementation of the drag and drop feature
The current method used to hide card being dragged by telling a pile to hide it's top card has some drawbacks.
It 

## Version 0.0.1
Good enough to share 
has drag and drop but no proper logic
