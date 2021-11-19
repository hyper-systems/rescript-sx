# sx - styling extensions

[![npm version](https://img.shields.io/npm/v/@hyper-systems/rescript-sx.svg)](https://www.npmjs.com/package/@hyper-systems/rescript-sx)
[![license](https://img.shields.io/npm/l/@hyper-systems/rescript-sx.svg)](https://www.npmjs.com/package/@hyper-systems/rescript-sx)

Sx is a just-in-time CSS generator based on the Tailwind API.

```rescript
module Button = {
  @react.component
  let make = () => {
    <button
      className=%sx(`p-2 bg-white border(1 purple-700) text(lg center black) hover:(bg-purple-700 text-white)`)>
      {React.string("Click!")}
    </button>
  }
}
```

The `%sx(...)` ppx will generate a static CSS file during the build phase with the following content:

```css
.bg-white {
  background-color: #fff;
}
.border-1 {
  border-width: 1px;
}
.border-purple-700 {
  border-color: #6b46c1;
}
.hover\:bg-purple-700:hover {
  background-color: #6b46c1;
}
.hover\:text-white:hover {
  color: #fff;
}
.p-2 {
  padding: 0.5rem;
}
.text-black {
  color: #000;
}
.text-center {
  text-align: center;
}
.text-lg {
  font-size: 1.125rem;
}
```

## Installation

Install the package:
```
$ npm i @hyper-systems/rescript-sx@nightly
```

Enable the ppx in `bsconfig.json`:
```json
  "ppx-flags": [
    "sx_ppx --output=sx.gen.css"
  ]
```

This will generate a static CSS file called `sx.gen.css` that can be loaded directly in your HTML.
