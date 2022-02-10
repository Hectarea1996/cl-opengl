
### glActiveShaderProgram -> active-shader-program

```
(active-shader-program pipeline program)
```

### glActiveTexture -> active-texture

```
(active-texture texture)
```

### glAttachShader -> attach-shader

```
(attach-shader program shader)
```

### glBeginConditionalRender -> begin-conditional-render

```
(begin-conditional-render id mode)
```

### glBeginQuery -> begin-query

```
(begin-query target id)
```

### glBeginQueryIndexed -> begin-query

```
(begin-query-indexed target index id)
```

### glBeginTransformFeedback -> begin-transform-feedback

```
(begin-transform-feedback primitiveMode)
```

### glBindAttribLocation -> bind-attrib-location

```
(bind-attrib-location program index name)
```

### glBindBuffer -> bind-buffer

```
(bind-buffer target buffer)
```

### glBindBufferBase -> bind-buffer-base

```
(bind-buffer-base target index buffer)
```

### glBindVertexArray -> bind-vertex-array

```
(bind-vertex-array array)
```

### glBufferData -> buffer-data

```
(buffer-data target usage data &key (offset 0))
```

* **Note**: *data* is a gl-array.
* **Note**: *offset* is optional.

### glClear -> clear

```
(clear mask)
```

### glClearColor -> clear-color

```
(clear-color red green blue alpha)
```

### glCompileShader -> compile-shader

```
(compile-shader shader)
```

### glCreateProgram -> create-program

```
(create-program) => program
```

### glCreateShader -> create-shader

```
(create-shader shaderType) => shader
```

### glDeleteShader -> delete-shader

```
(delete-shader shader)
```

### glDisableVertexArrayAttrib -> disable-vertex-array-attrib

```
(disable-vertex-array-attrib vaobj index)
```

### glDisableVertexAttribArray -> disable-vertex-attrib-array

```
(disable-vertex-attrib-array index)
```

### glDrawArrays -> draw-arrays

```
(draw-arrays mode first count)
```

### glDrawElements -> draw-elements

```
(draw-elements mode array &key (count (gl-array-size array)) (indices 0))
```

* **Note**: *array* must be a gl-array.

### glEnableVertexArrayAttrib -> enable-vertex-array-attrib

```
(enable-vertex-array-attrib vaobj index)
```

### glEnableVertexAttribArray -> enable-vertex-attrib-array

```
(enable-vertex-attrib-array index)
```

### glEndConditionalRender -> end-conditional-render

```
(end-conditional-render)
```

### glEndQuery -> end-query

```
(end-query target)
```

### glEndQueryIndexed -> end-query-indexed

```
(end-query-indexed target index)
```

### glEndTransformFeedback -> end-transform-feedback

```
(end-transform-feedback)
```

### glGenBuffers -> gen-buffers | gen-buffer

```
(gen-buffers n) => buffers
(gen-buffer) => buffer
```

* **Note**: *buffers* is a list.

### glGenVertexArrays -> gen-vertex-arrays | gen-vertex-array

```
(gen-vertex-arrays n) => arrays
(gen-vertex-array) => array
```

* **Note**: *arrays* is a list.

### glGetProgramInfoLog -> get-program-info-log

```
(get-program-info-log program) => infoLog
```

### glGetProgramiv -> get-program

```
(get-program program pname) => params
```

* **Note**: *params* will be a boolean if *pname* is one of these values: `:delete-status` `:link-status` `:validate-status` `:completion-status-khr`. Otherwise, an int is returned.

### glGetShaderInfoLog -> get-shader-info-log

```
(get-shader-info-log shader) => infoLog
```

### glGetShaderiv -> get-shader

```
(get-shader shader pname) => params
```

* **Note**: *params* is a boolean when one of the next pnames is used: `:delete-status`, `:compile-status`, `:completion-status-khr`. Otherwise, an int is returned.

### glLinkProgram -> link-program

```
(link-program program)
```

### glNamedBufferData -> named-buffer-data

```
(named-buffer-data buffer usage data &key (offset 0))
```

* **Note**: *data* is a gl-array.
* **Note**: *offset* is optional.

### glPolygonMode -> polygon-mode

```
(polygon-mode face mode)
```

### glShaderSource -> shader-source

```
(shader-source shader string)
```

* **Note**: *string* must be a string or a list of strings.

### glUseProgram -> use-program

```
(use-program program)
```

### glVertexAttribIPointer -> vertex-attrib-ipointer

```
(vertex-attrib-ipointer index size type stride pointer)
```

* **Note**: pointer must be an integer.

### glVertexAttribLPointer -> vertex-attrib-lpointer

```
(vertex-attrib-lpointer index size type stride pointer)
```

* **Note**: pointer must be an integer.

### glVertexAttribPointer -> vertex-attrib-pointer

```
(vertex-attrib-pointer index size type normalized stride pointer)
```

* **Note**: normalized is of type boolean.

* **Note**: pointer must be an integer.

### glViewport -> viewport

```
(viewport x y width height)
```
