```
                    Source program
                          |
                          v
                        Parser
                      (Chapter 1)
                          |
                          v
                    Lambda lifter
                     (Chapter 6)
                          |
                          v
                     Core program
                      |  |  |  |
     +----------------+  |  |  +----------------+
     |                   |  |                   |
     |            +------+  +---+               |
     |            |             |               |
     v            v             v               v
 Template     G-machine        TIM       Parallel G-machine
 compiler     compiler       compiler       compiler
    |             |             |               |
    v             v             v               v
 Template     G-machine        TIM       Parallel G-machine
interpreter  interpreter   interpreter      interpreter
(Chapter 2)  (Chapter 3)   (Chapter 4)      (Chapter 5)
```
