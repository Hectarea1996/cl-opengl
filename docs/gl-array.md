
# gl-array

Many functions accepts array data and OpenGL must know the size of the type is being used into. To make programming easier, cl-opengl provides a wrapper around C arrays, gl-array.

* [Functions](https://hectarea1996.github.io/cl-opengl/gl-array.html#functions)
* [Macros](https://hectarea1996.github.io/cl-opengl/gl-array.html#macros)

## Functions

* [alloc-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#alloc-gl-array): Allocates an array.
* [free-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#free-gl-array): Frees an array.
* [make-null-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#make-null-gl-array): Creates a null gl-array.
* [gl-array-byte-size](https://hectarea1996.github.io/cl-opengl/gl-array.html#gl-array-byte-size): Returns the number of bytes of an array.
* [glaref](https://hectarea1996.github.io/cl-opengl/gl-array.html#glaref): Retrieves an element from an array.

## Macros

* [with-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#with-gl-array)

## Function documentation

### alloc-gl-array

```
(alloc-gl-array type count) => array
```

Creates an array with `count` elements of type `type`.

* **Parameters**:
  * **type**: The [type](https://hectarea1996.github.io/cl-opengl/primitive-types.html) of the elements of the created array.
  * **count**: The size of the array.

* **Returns**:
  * **array**: The created array.

* **See also**: [free-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#free-gl-array).

### free-gl-array

```
(free-gl-array array)
```

Frees an array allocated by [alloc-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#alloc-gl-array).

* **Parameters**:
  * **array**: The array to be freed.

* **See also**: [alloc-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#alloc-gl-array).

### make-null-gl-array

```
(make-null-gl-array type) => array
```

Returns a gl-array with a size of 0. It represents a NULL pointer.

* **Parameters**:
  * **type**: The type of the elements of the returned array.

* **Returns**:
  * **array**: The created array.

* **Notes**: The created array must not be freed by [free-gl-array](https://hectarea1996.github.io/cl-opengl/gl-array.html#free-gl-array).

### gl-array-byte-size

```
(gl-array-byte-size array) => bytes
```

Returns the number of bytes in the array.

* **Parameters**:
  * **array**: The array to retrive its bytes size.

* **Returns**:
  * **bytes**: The number of bytes in the array. 

### glaref

```
(glaref array index &optional (component nil)) => element
```

Returns the `index`-th component of `array`. If `component` is
supplied and `array` is of a compound type the slot named
`component` is returned.

* **Parameters**:
  * **array**: The array to retrieve the element.
  * **index**: The index where the element to retrieve is.
  * **component**: A name symbol designating the slot inside the element.

* **Returns**:
  * **element**: The element of `array` in the `index`-th position. If `component` is supplied and array is of a compound type, the slot named `component` is returned.

## Macro documentation

### with-gl-array

```
(with-gl-array (var type &key count) &body body)
```

Allocates a fresh gl-aarray of type `type` and `count` elements.
The array will be bound to `var` and is freed when execution moves
outside `with-gl-array`.

