# LBVirtualKeyboard

**LBVirtualKeyboard** is a modular, highly configurable virtual keyboard designed for industrial touchscreen applications. It was created to meet the need for a flexible input system that adapts to specific operational contexts.

---

## 🚀 Key Features

- **Fully customizable layouts** via XML files
- **Unlimited pagination**: define as many pages as needed
- **Context-specific key sets**: display only the keys required for a given task
- **Multi-OS support**: compatible with Windows and X11/Linux
- **Modular architecture**: clean separation between logic, input, and layout
- **Extensibility**: support for additional features via plugin modules

---

## 🧱 Project Structure

| Module               | Description                                                  |
|----------------------|--------------------------------------------------------------|
| `uLBVirtualKeyboard` | Core module: rendering, event handling, layout management    |
| `Base_KeyInput`      | Abstract base class for keyboard input handling              |
| `win_keyinput`       | Windows-specific input implementation                        |
| `X11_KeyInput`       | X11/Linux-specific input implementation                      |
| `uVKExt`             | Extension module for advanced features                       |
| `Numeric_Keyboard.xml` | Example numeric layout in XML format                       |
| `Keyboard_Layout.xml` | Example alphanumeric layout with Shift and pagination       |

---

## 📄 XML Layouts

Layouts are defined using readable and editable XML files. Here's a basic structure:

```xml
<KeyBoard>
  <Pages>
    <Page>
      <Row>
        <Key>
          <Values Default="a" Shift="A" />
        </Key>
        <Key kType="BackSpace" />
        <Key kType="Return" />
      </Row>
    </Page>
  </Pages>
</KeyBoard>
```

## Supported Attributes

Each `<Key>` element in the XML layout can include the following attributes:

- **Default**:  
  The character displayed and inserted when no modifier is active.  
  Example: `<Values Default="a" />`

- **Shift**:  
  The character displayed and inserted when the Shift key is active.  
  Example: `<Values Default="a" Shift="A" />`

- **kType**:  
  Defines the key as a special function key. Supported types include:  
  - `BackSpace`  
  - `Return`  
  - `Space`  
  - `Shift`  
  - `Canc`  
  - `Left`, `Right`, `Up`, `Down`  
  - `Switcher` (used to switch between pages)

- **Empty `<Key />`**:  
  Used for layout spacing or placeholder keys. These keys are visually rendered but do not produce input.

## Dynamic Key Behavior

LBVirtualKeyboard supports dynamic key rendering and input based on modifier states:

- **Shift Support**  
  Keys can define alternate values for when the Shift key is active.  
  Both the label and the inserted character change accordingly.

- **Page Switching**  
  The `Switcher` key allows toggling between multiple layout pages, enabling context-specific input modes.


## Supported Environments

LBVirtualKeyboard is compatible with multiple operating systems:

- **Windows**  
  Input handling is implemented via the `win_keyinput` module.

- **Linux / X11**  
  Input handling is implemented via the `X11_KeyInput` module.

- **Other Platforms**  
  Additional environments (e.g. Wayland, embedded systems) can be supported by extending the `Base_KeyInput` class.

The modular design ensures easy portability and integration across different systems.


## 🎨 Theme System (Experimental)

LBVirtualKeyboard includes an **initial implementation of a theme system** that allows visual customization of the keyboard and its keys.

### Key Capabilities

- Define **visual styles** for different key types: normal, special, modifier, space
- Customize:
  - Background and border colors
  - Font style and size
  - Corner radius and shadow
  - Pressed state appearance
  - Rendering style (flat, raised, gradient, glass)
- Themes are defined via **JSON files** and loaded dynamically

### Structure

Themes are managed by the `uLBKeyboardThemes` module and consist of:

- `TKeyThemeData`: defines the appearance of individual keys
- `TKeyboardTheme`: groups key styles and global settings (background, animation)
- `TThemeManager`: handles loading, switching, and storing multiple themes


