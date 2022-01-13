
# Rendering

## Functions

* [clear](https://hectarea1996.github.io/cl-opengl/rendering.html#clear): Clear buffers to preset values.

* [clear-color](https://hectarea1996.github.io/cl-opengl/rendering.html#clear-color): Specify clear values for the color buffers.

## Function documentation

### clear

```
(clear mask)
```

`clear` sets the bitplane area of the window to values previously selected by `clear-color`, `clear-depth`, and `clear-stencil`. Multiple color buffers can be cleared simultaneously by selecting more than one buffer at a time using [draw-buffer](https://hectarea1996.github.io/cl-opengl/rendering.html#draw-buffer).

The pixel ownership test, the scissor test, dithering, and the buffer writemasks affect the operation of `clear`. The scissor box bounds the cleared region. Alpha function, blend function, logical operation, stenciling, texture mapping, and depth-buffering are ignored by `clear`.

`clear` takes a single argument that is the bitwise OR of several values indicating which buffer is to be cleared.

The values are as follows:

* `:color-buffer-bit` Indicates the buffers currently enabled for color writing.

* `:depth-buffer-bit` Indicates the depth buffer.

* `:stencil-buffer-bit` Indicates the stencil buffer.

The value to which each buffer is cleared depends on the setting of the clear value for that buffer.

* **Parameters**:
  * **mask**: Bitwise OR of masks that indicate the buffers to be cleared. The three masks are `:color-buffer-bit`, `:depth-buffer-bit`, and `:stencil-buffer-bit`.

* **Notes**: If a buffer is not present, then a `clear` directed at that buffer has no effect.

* **Errors**: `:invalid-value` is generated if any bit other than the three defined bits is set in `mask`.

* **Associated gets**:
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:depth-clear-value`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:color-clear-value`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:stencil-clear-value`.

* **See also**: `clear-color`, `clear-depth`, `clear-stencil`, [color-mask](https://hectarea1996.github.io/cl-opengl/state-management.html#color-mask), [depth-mask](https://hectarea1996.github.io/cl-opengl/state-management.html#depth-mask), [draw-buffer](https://hectarea1996.github.io/cl-opengl/rendering.html#draw-buffer), [scissor](https://hectarea1996.github.io/cl-opengl/state-management.html#scissor), [stencil-mask](https://hectarea1996.github.io/cl-opengl/state-management.html#stencil-mask).

### clear-color

```
(clear-color red green blue alpha)
```

`clear-color` specifies the red, green, blue, and alpha values used by [clear](https://hectarea1996.github.io/cl-opengl/rendering.html#clear) to clear the color buffers. Values specified by `clear-color` are clamped to the range [0,1].

* **Parameters**:
  * **red,green,blue,alpha**: Specify the red, green, blue, and alpha values used when the color buffers are cleared. The initial values are all 0.

* **Associated gets**:
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:color-clear-value`.

* **See also**: [clear](https://hectarea1996.github.io/cl-opengl/rendering.html#clear).