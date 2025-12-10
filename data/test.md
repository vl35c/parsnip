# Renderer

The renderer is used to hold drawing functions that may not be called in the correct order, and draw them in the correct order at the end of the frame.

### PROPERTIES:
`held_functions` - list of functions held by the renderer waiting to be called

### METHODS:
`call(self)` - calls all the waiting functions in order of their `z` value

---
`flush(self)` - clears the list of held functions

---
`hold(self, func: Callable, z: float)` - holds a function and a `z` layer - which is the order the function is to be called\
\
`func` - the function to be called\
`z` - layer to call function - lower means called first

---
