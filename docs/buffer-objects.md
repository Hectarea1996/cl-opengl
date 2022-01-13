
# Buffer objects

## Functions

* [bind-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer): Bind a named buffer object.
* [gen-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#gen-buffers): Generate buffer object name.
* [gen-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#gen-buffers): Generate buffer object names.

## Function documentation

### bind-buffer

```
(bind-buffer target buffer)
```

`bind-buffer` binds a buffer object to the specified buffer binding point. Calling `bind-buffer` with *target* set to one of the accepted symbolic constants and *buffer* set to the name of a buffer object binds that buffer object name to the target. If no buffer object with name *buffer* exists, one is created with that name. When a buffer object is bound to a target, the previous binding for that target is automatically broken.

Buffer object names are unsigned integers. The value zero is reserved, but there is no default buffer object for each buffer object target. Instead, *buffer* set to zero effectively unbinds any buffer object previously bound, and restores client memory usage for that buffer object target (if supported for that target). Buffer object names and the corresponding buffer object contents are local to the shared object space of the current GL rendering context; two rendering contexts share buffer object names only if they explicitly enable sharing between contexts through the appropriate GL windows interfaces functions.

[gen-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#gen-buffers) must be used to generate a set of unused buffer object names.

The state of a buffer object immediately after it is first bound is an unmapped zero-sized memory buffer with `:read-write` access and `:static-draw` usage.

While a non-zero buffer object name is bound, GL operations on the target to which it is bound affect the bound buffer object, and queries of the target to which it is bound return state from the bound buffer object. While buffer object name zero is bound, as in the initial state, attempts to modify or query state on the target to which it is bound generates an `:invalid-operation` error.

When a non-zero buffer object is bound to the `:array-buffer` target, the vertex array pointer parameter is interpreted as an offset within the buffer object measured in basic machine units.

When a non-zero buffer object is bound to the `:draw-indirect-buffer` target, parameters for draws issued through [draw-arrays-indirect](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-arrays-indirect) and [draw-elements-indirect](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-elements-indirect) are sourced from the specified offset in that buffer object's data store.

When a non-zero buffer object is bound to the `:dispatch-indirect-buffer` target, the parameters for compute dispatches issued through [dispatch-compute-indirect](https://hectarea1996.github.io/cl-opengl/utility.html#dispatch-compute-indirect) are sourced from the specified offset in that buffer object's data store.

While a non-zero buffer object is bound to the `:element-array-buffer` target, the indices parameter of [draw-elements](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-elements), [draw-elements-instanced](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-elements-instanced), [draw-elements-base-vertex](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-elements-base-vertex), [draw-range-elements](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-range-elements), [draw-range-elements-base-vertex](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#draw-range-elements-base-vertex), [multi-draw-elements](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#multi-draw-elements), or [multi-draw-elements-base-vertex](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#multi-draw-elements-base-vertex) is interpreted as an offset within the buffer object measured in basic machine units.

While a non-zero buffer object is bound to the `:pixel-pack-buffer` target, the following commands are affected: [get-compressed-tex-image](https://hectarea1996.github.io/cl-opengl/textures.html#get-compressed-tex-image), [get-tex-image](https://hectarea1996.github.io/cl-opengl/textures.html#get-tex-image), and [read-pixels](https://hectarea1996.github.io/cl-opengl/rendering.html#read-pixels). The pointer parameter is interpreted as an offset within the buffer object measured in basic machine units.

While a non-zero buffer object is bound to the `:pixel-unpack-buffer` target, the following commands are affected: [compressed-tex-image-1d](https://hectarea1996.github.io/cl-opengl/textures.html#compressed-tex-image-1d), [compressed-tex-image-2d](https://hectarea1996.github.io/cl-opengl/textures.html#compressed-tex-image-2d), [compressed-tex-image-3d](https://hectarea1996.github.io/cl-opengl/textures.html#compressed-tex-image-3d), [compressed-tex-sub-image-1d](https://hectarea1996.github.io/cl-opengl/textures.html#compressed-tex-sub-image-1d), [compressed-tex-sub-image-2d](https://hectarea1996.github.io/cl-opengl/textures.html#compressed-tex-sub-image-2d), [compressed-tex-sub-image-3d](https://hectarea1996.github.io/cl-opengl/textures.html#compressed-tex-sub-image-3d), [tex-image-1d](https://hectarea1996.github.io/cl-opengl/textures.html#tex-image-1d), [tex-image-2d](https://hectarea1996.github.io/cl-opengl/textures.html#tex-image-2d), [tex-image-3d](https://hectarea1996.github.io/cl-opengl/textures.html#tex-image-3d), [tex-sub-image-1d](https://hectarea1996.github.io/cl-opengl/textures.html#tex-sub-image-1d), [tex-sub-image-2d](https://hectarea1996.github.io/cl-opengl/textures.html#tex-sub-image-2d), and [tex-sub-image-3d](https://hectarea1996.github.io/cl-opengl/textures.html#tex-sub-image-3d). The pointer parameter is interpreted as an offset within the buffer object measured in basic machine units.

The buffer targets `:copy-read-buffer` and `:copy-write-buffer` are provided to allow [copy-buffer-sub-data](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#copy-buffer-sub-data) to be used without disturbing the state of other bindings. However, [copy-buffer-sub-data](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#copy-buffer-sub-data) may be used with any pair of buffer binding points.

The `:transform-feedback-buffer` buffer binding point may be passed to `bind-buffer`, but will not directly affect transform feedback state. Instead, the indexed `:transform-feedback-buffer` bindings must be used through a call to [bind-buffer-base](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer-base) or [bind-buffer-range](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer-range). This will affect the generic `:transform-feedback-buffer` binding.

Likewise, the `:uniform-buffer`, `:atomic-counter-buffer` and `:shader-storage-buffer` buffer binding points may be used, but do not directly affect uniform buffer, atomic counter buffer or shader storage buffer state, respectively. [bind-buffer-base](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer-base) or [bind-buffer-range](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer-range) must be used to bind a buffer to an indexed uniform buffer, atomic counter buffer or shader storage buffer binding point.

The `:query-buffer` binding point is used to specify a buffer object that is to receive the results of query objects through calls to the [get-query-object](https://hectarea1996.github.io/cl-opengl/queries.html#get-query-object) family of commands.

A buffer object binding created with `bind-buffer` remains active until a different buffer object name is bound to the same target, or until the bound buffer object is deleted with [delete-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#delete-buffers).

Once created, a named buffer object may be re-bound to any target as often as needed. However, the GL implementation may make choices about how to optimize the storage of a buffer object based on its initial binding target.

* **Parameters**:
  * **target**: Specifies the target to which the buffer object is bound, which must be one of the buffer binding targets in the following table:
  | **Buffer Binding Target** | **Purpose** |
  | ------------------------- | ----------- |
  | `:array-buffer` | Vertex attributes |
  | `:atomic-counter-buffer` | Atomic counter storage |
  | `:copy-read-buffer` | Buffer copy source |
  | `:copy-write-buffer` | Buffer copy destination |
  | `:dispatch-indirect-buffer` | Indirect compute dispatch commands |
  | `:draw-indirect-buffer` | Indirect command arguments |
  | `:element-array-buffer` | Vertex array indices |
  | `:pixel-pack-buffer` | Pixel read target |
  | `:pixel-unpack-buffer` | Texture data source |
  | `:query-buffer` | Query result buffer |
  | `:shader-storage-buffer` | Read-write storage for shaders |
  | `:texture-buffer` | Texture data buffer |
  | `:transform-feedback-buffer` | Transform feedback buffer |
  | `:uniform-buffer` | Uniform block storage |
  * **buffer**: Specifies the name of a buffer object.

* **Errors**: `:invalid-enum` is generated if *target* is not one of the allowable values.

`:invalid-value` is generated if *buffer* is not a name previously returned from a call to [gen-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#gen-buffers).

* **Associated gets**:
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:array-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:atomic-counter-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:copy-read-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:copy-write-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:draw-indirect-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:dispatch-indirect-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:element-array-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:pixel-pack-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:pixel-unpack-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:shader-storage-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:transform-feedback-buffer-binding`.
  * [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get) with argument `:uniform-buffer-binding`.

* **See also**: [gen-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#gen-buffers), [bind-buffer-base](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer-base), [bind-buffer-range](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer-range), [map-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#map-buffer), [unmap-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#unmap-buffer), [delete-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#delete-buffers), [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get), [is-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#is-buffer).

### gen-buffers

```
(gen-buffer) => buffer
(gen-buffers n) => buffers
```

`gen-buffers` returns *n* buffer object names. There is no guarantee that the names form a contiguous set of integers; however, it is guaranteed that none of the returned names was in use immediately before the call to `gen-buffers`.

Buffer object names returned by a call to `gen-buffers` are not returned by subsequent calls, unless they are first deleted with [delete-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#delete-buffers).

No buffer objects are associated with the returned buffer object names until they are first bound by calling [bind-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer).

* **Parameters**:
  * **n**: An integer specifing the number of buffer object names to be generated.

* **Returns**:
  * **buffer**: A buffer object name.
  * **buffers**: A list with the generated buffer object names.

* **Errors**: `:invalid-value` is generated if *n* is negative.

* **Associated gets**: [is-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#is-buffer).

* **See also**: [bind-buffer](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#bind-buffer), [delete-buffers](https://hectarea1996.github.io/cl-opengl/buffer-objects.html#delete-buffers), [get](https://hectarea1996.github.io/cl-opengl/state-management.html#get)