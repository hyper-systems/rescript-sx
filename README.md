# sx - styling extensions

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
