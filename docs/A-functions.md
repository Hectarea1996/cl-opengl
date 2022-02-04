
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

### glShaderSource -> shader-source

```
(shader-source shader string)
```

* **Note**: *string* must be a string or a list of strings.

### glUseProgram -> use-program

```
(use-program program)
```

### glViewport -> viewport

```
(viewport x y width height)
```
