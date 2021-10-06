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
