
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

When a non-zero buffer object is bound to the GL_ARRAY_BUFFER target, the vertex array pointer parameter is interpreted as an offset within the buffer object measured in basic machine units.

When a non-zero buffer object is bound to the GL_DRAW_INDIRECT_BUFFER target, parameters for draws issued through glDrawArraysIndirect and glDrawElementsIndirect are sourced from the specified offset in that buffer object's data store.

When a non-zero buffer object is bound to the GL_DISPATCH_INDIRECT_BUFFER target, the parameters for compute dispatches issued through glDispatchComputeIndirect are sourced from the specified offset in that buffer object's data store.

While a non-zero buffer object is bound to the GL_ELEMENT_ARRAY_BUFFER target, the indices parameter of glDrawElements, glDrawElementsInstanced, glDrawElementsBaseVertex, glDrawRangeElements, glDrawRangeElementsBaseVertex, glMultiDrawElements, or glMultiDrawElementsBaseVertex is interpreted as an offset within the buffer object measured in basic machine units.

While a non-zero buffer object is bound to the GL_PIXEL_PACK_BUFFER target, the following commands are affected: glGetCompressedTexImage, glGetTexImage, and glReadPixels. The pointer parameter is interpreted as an offset within the buffer object measured in basic machine units.

While a non-zero buffer object is bound to the GL_PIXEL_UNPACK_BUFFER target, the following commands are affected: glCompressedTexImage1D, glCompressedTexImage2D, glCompressedTexImage3D, glCompressedTexSubImage1D, glCompressedTexSubImage2D, glCompressedTexSubImage3D, glTexImage1D, glTexImage2D, glTexImage3D, glTexSubImage1D, glTexSubImage2D, and glTexSubImage3D. The pointer parameter is interpreted as an offset within the buffer object measured in basic machine units.

The buffer targets GL_COPY_READ_BUFFER and GL_COPY_WRITE_BUFFER are provided to allow glCopyBufferSubData to be used without disturbing the state of other bindings. However, glCopyBufferSubData may be used with any pair of buffer binding points.

The GL_TRANSFORM_FEEDBACK_BUFFER buffer binding point may be passed to glBindBuffer, but will not directly affect transform feedback state. Instead, the indexed GL_TRANSFORM_FEEDBACK_BUFFER bindings must be used through a call to glBindBufferBase or glBindBufferRange. This will affect the generic GL_TRANSFORM_FEEDBACK_BUFFER binding.

Likewise, the GL_UNIFORM_BUFFER, GL_ATOMIC_COUNTER_BUFFER and GL_SHADER_STORAGE_BUFFER buffer binding points may be used, but do not directly affect uniform buffer, atomic counter buffer or shader storage buffer state, respectively. glBindBufferBase or glBindBufferRange must be used to bind a buffer to an indexed uniform buffer, atomic counter buffer or shader storage buffer binding point.

The GL_QUERY_BUFFER binding point is used to specify a buffer object that is to receive the results of query objects through calls to the glGetQueryObject family of commands.

A buffer object binding created with glBindBuffer remains active until a different buffer object name is bound to the same target, or until the bound buffer object is deleted with glDeleteBuffers.

Once created, a named buffer object may be re-bound to any target as often as needed. However, the GL implementation may make choices about how to optimize the storage of a buffer object based on its initial binding target.

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