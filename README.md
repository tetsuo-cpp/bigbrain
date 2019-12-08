# Bigbrain
A Brainfuck interpreter written in Common Lisp.
## Dependencies
* Roswell.
* ASDF.
## Setup
ASDF needs to be configured to see the Bigbrain system definition in order for the `bigbrain.ros` script to work. There are a number of ways of doing this but my chosen way is to place a symlink to the `bigbrain.asd` file under `~/common-lisp/`.
## Usage
The interpreter can be run as a script.
```
./bigbrain.ros <file>
```
Or compiled into a binary.
```
ros build bigbrain.ros
./bigbrain <file>
```
