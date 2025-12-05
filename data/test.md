# Font

Font overrides pygame's own font class, extending its functionality to be able to render and draw in 1 function call, increasing code neatness.

### PROPERTIES:
`font` - the font face that the class uses\
`window` - the global window that the program uses

### METHODS:
`render(self, text: str, antialias: bool, color: Color | str | tuple, pos: tuple[int, int]) -> pygame.Surface` - renders string onto the screen - overriding the builtin pygame font.render\

`str` - string passed into render to be displayed\
`antialias` - should the string be antialiased\
`color` - the color for the text to be rendered in - examples - `Color.BLACK`, `"black"`, `(255, 255, 255)`\
`pos` - the position on the window for the text to be rendered

---
`string_to_surface(self, text: str) -> pygame.Surface` - converts a string into a pygame.Surface\
\
`str` - the string to be converted into surface

---

# Graph

A graph is a chart which allows users to be able to visualise stock data on a candle graph

### PROPERTIES:
`font` - local instance of the Font class\
`height` - height of the graph\
`mouse_handler` - global MouseHandler\
`rect` - the encompassing pygame.Rect of the graph\
`renderer` - local instance of the Renderer class\
`width` - width of the graph\
`window` - the global window that the program uses\
`x` - x coordinate of the top left of the graph\
`y` - y coordinate of the top left of the graph\

### METHODS:
`__add_tuples(t1: tuple, t2: tuple) -> tuple[int, int]` - adds two tuples together\
\
`t1` - tuple 1\
`t2` - tuple 2

---
`__candle_color(top: int, bottom: int) -> str` - calculates the color of a candle based on the position of the top and bottom pixels on the graph\
\
`top` - top coordinate on the graph\
`bottom` - bottom coordinate on the graph

---
`__candle_data_calculate(self, stock_data: object, days: list[int])` - calculates the pricing data (opening price, closing price, period high, and period low) for a non-zero list of days\
\
`stock_data` - contains data about stocks, and current day\
`days` - list of the days for the data to be calculated over

---
`__clamp(min_value: int, max_value: int, value: int) -> int` - returns a passed in value bounded to an upper and lower limit\
\
`min_value` - lower limit of bound\
`max_value` - upper limit of bound\
`value` - value to be bounded

---
`__get_column(x: int) -> int` - returns the column of the candle graph for a value x\
\
`x` - integer value to be converted

---
`candle(self, stock_data: object)` - draws a candle on the graph\
\
`stock_data` - contains data about stocks, and current day

---
`candle_data(self, days: list[int], stock_data: object)` - fetches the candle data and then displays it on the screen\
\
`days` - list of days for the data to be calculated over\
`stock_data` - contains data about stocks, and current day

---
`draw(self, stock_data: object)` - calls the functions to draw the graph\
\
`stock_data` - contains data about stocks, and current day

---
`draw_base(self)` - draws the background of the graph

---
`draw_data(self, stock_data: object)` - draws the candles on the graph\
\
`stock_data` - contains data about stocks, and current day

---
`draw_line(self, value: float, stock_data: object)` - draws single line from `draw_lines` function\
\
`value` - value to be mapped and drawn onto the graph\
`stock_data` - contains data about stocks, and current day

---
`draw_lines(self, stock_data: object)` - draws pricing guidance lines on the graph\
\
`stock_data` - contains data about stocks, and current day

---
`handle_held(self, stock_data: object)` - called when the mouse is held on the graph, and calls functions to highlight and display pricing data for the selected period of time\
\
`stock_data` - contains data about stocks, and current day

---
`highlight_candle(self, start_position: int, end_position: int = -1)` - if end position is -1, highlights a single candle, otherwise highlights a range of candles\
\
`start_position` - leftmost candle to highlight\
`end_position` - rightmost candle to highlight

---
`hover(self, stock_data: object)` - if the mouse hovers over a candle this function highlights and displays the data for that candle\
\
`stock_data` - contains data about stocks, and current day

---
`map(min_value: int, max_value: int, min_y: int, max_y: int, value: int) -> int` - takes a graph and maps the bottom and top values to the pixel values on screen, then works out where the `value` pixel would be (lerp)\
\
`min_value` - minimum value on graph\
`max_value` - maximum value on graph\
`min_y` - lowest y value of graph\
`max_y` - highest y value of graph\
`value` - value to be mapped onto graph

---

# Interactor

An interactor is an input that the user is able to interact with.

### PROPERTIES
`_func` - function attached to the interactor\
`bg_color` - background color of the interactor\
`font` - local instance of the Font class\
`height` - height of the interactor\
`pos` - position of the top left of the interactor\
`radius` - corner radius of the interactor\
`rect` - the pygame.Rect encompassing the interactor\
`text` - string to be rendered on the interactor\
`text_color` - color of the string to be rendered on the interactor\
`width` - width of the interactor\
`window` - the global window that the program uses\
`x` - x position of the top left of the interactor\
`y` - y position of the top left of the interactor

### METHODS
`_centered_text(self, text_object: str) -> tuple[int, int]` - returns the position to place text so that it is centered in the interactor\
\
`text_object` - string to be centered

---
`display(self)` - calls the draw and render function in 1 call

---
`draw(self)` - draws the interactor

---
`func(self) -> ()` - calls the function attached to the interactor

---
`render(self)` - renders the text on the interactor

---

# Renderer

The renderer is used to hold drawing functions that may not be called in the correct order, and draw them in the correct order at the end of the frame.

### PROPERTIES
`held_functions` - list of functions held by the renderer waiting to be called

### METHODS
`call(self)` - calls all the waiting functions in order of their `z` value

---
`flush(self)` - clears the list of held functions

---
`hold(self, func: Callable, z: float)` - holds a function and a `z` layer - which is the order the function is to be called\
\
`func` - the function to be called\
`z` - layer to call function - lower means called first

---

# TextInput(Interactor)

TextInput allows the user to input text into a custom interactor

### PROPERTIES (EXTENSIONS)
`__buttons` - list of buttons which gets populated when using the text input\
`active` - boolean showing whether the text box is active or not\
`button_func` - the function that the buttons created when using the text input call\
`input_func` - the function called when the text input is changed\
`keyboard` - the local KeyboardHandler\
`on_input_list_update` - function which is called when the input is updated\
`placeholder` - placeholder text that shows when the text input is not active

### METHODS (EXTENSIONS)
`__offset_text(self, text_object: str) -> tuple[int, int]` - returns a tuple containing the position to place offset text inside of the text input\
\
`text_object` - string to be offset

---
`activate(self)` - sets the button's state to active

---
`deactivate(self)` - sets the button's state to inactive and resets some other fields

---
`get_buttons(self) -> list[Button]` - returns a list of all of the generated buttons

---
`handle_key_press(self, key: object) -> list[Button]` - takes in a pressed key and handles it\
\
`key` - either a pygame.K_BACKSPACE, or a character to be typed

---
`key_down(self, event: object)` - handles a key press event passed in by the KeyboardHandler\
\
`event` - the key down event object

---
`on_type(self) -> list[Button]` - appends the users input to the text property and then calls the `on_inputlist_update` function 

---
`render(self)` - draws the text input interactor

---
`remove_char(self)` - removes a character from the text property

---
`set_keyboard_handler(self, keyboard: KeyboardHandler)` - sets the local keyboard handler for the class\
\
`keyboard` - the KeyboardHandler object to assign to the class

---

