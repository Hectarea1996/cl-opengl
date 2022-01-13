
# State management

## Functions

*[get](https://hectarea1996.github.io/cl-opengl/state-management.html#get): Return the value or values of a selected parameter.

* [viewport](https://hectarea1996.github.io/cl-opengl/state-management.html#viewport): Set the viewport.

## Function documentation

### viewport

```
(viewport x y width height)
```

`viewport` specifies the affine transformation of `x` and `y` from normalized device coordinates to window coordinates. Let (xnd,ynd) be normalized device coordinates. Then the window coordinates (xw,yw) are computed as follows:

xw = (xnd+1)(width/2)+x
yw = (ynd+1)(height/2)+y

Viewport width and height are silently clamped to a range that depends on the implementation. To query this range, call [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:max-viewport-dims`.

* **Parameters**:
  * **x,y**: Specify the lower left corner of the viewport rectangle, in pixels. The initial value is (0,0).
  * **width,height**: Specify the width and height of the viewport. When a GL context is first attached to a window, `width` and `height` are set to the dimensions of that window.

* **Errors**: `:invalid-value` is generated if either `width` or `height` is negative.

* **Associated gets**:
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:viewport`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:max-viewport-dims`.

* **See also**: [depth-range](https://hectarea1996.github.io/cl-opengl/state-management.html#depth-range).