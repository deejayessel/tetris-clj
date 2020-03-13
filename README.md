# tetris-clj

An implementation of Tetris in Clojure.

## Abstractions

| Term | Definition                     
|:-----|:----------
| picture | a matrix of hashes used to visually represent a bitmap
| piece | a cycle of rotations and an index identifying the "current" rotation
| rotation | a particular orientation of a piece (e.g. T vs ㅗ)
| skirt | for a rotation, an array of the lowest y-coordinate for each x-value
| board | a matrix of occupied/unoccupied cells, augmented by metadata representing the current state of the game 


## TODO

### Essentials
- [ ] Randomly generate pieces
  - [ ] Add random ns
  - [ ] Add `get-random-piece`: choose piece randomly, choose random rotation index
  - [ ] Generate pieces randomly based on an input distribution
- [ ] Pieces drop over time (clock, ticks)
  - every tick, a piece drops, locks into place, or a new piece is added to its initial position
    to the board
- [ ] Drop a piece to the bottommost valid square `drop-piece`
- [ ] Detect when a game is over
- [ ] Detect line clears (for score calculation)
- [ ] Support single player
  - add player API to core_api ns

### Extras
- [ ] Score calculation
- [ ] Support multiple players

## License

Copyright © 2020 David J. Lee

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
