
# Primitive types

| OpenGL Type | Bitdepth | Description | Common enum |
| boolean | 1+ | A boolean value, either `t` or `nil` |  |
| byte | 8 | Signed, 2's complement binary integer | `:byte` |
| ubyte | 8 | Unsigned binary integer | `:unsigned-byte` |
| short | 16 | Signed, 2's complement binary integer | `:short` |
| ushort | 16 | Unsigned binary integer | `:unsigned-short` |
| int | 32 | Signed, 2's complement binary integer | `:int` |
| uint | 32 | Unsigned binary integer | `:unsigned-int` |
| fixed | 32 | Signed, 2's complement 16.16 integer | `:fixed` |
| int64 | 64 | Signed, 2's complement binary integer | |
| uint64 | 64 | Unsigned binary integer |  |
| sizei | 32 | A non-negative binary integer, for sizes. | |
| enum | 32 | An OpenGL enumerator value | |
| intptr | ptrbits 1 | Signed, 2's complement binary integer | |
| sizeiptr | ptrbits 1 | Non-negative binary integer size, for memory offsets and ranges ||
| sync | ptrbits 1 | [Sync Object](https://www.khronos.org/opengl/wiki/Sync_Object) handle | |
| bitfield | 32 | A bitfield value | |
| half | 16 | [An IEEE-754 floating-point value](https://www.khronos.org/opengl/wiki/Small_Float_Formats) | `:half-float` |
| float | 32 | An IEEE-754 floating-point value | `:float` |
| clampf | 32 | An IEEE-754 floating-point value, clamped to the range [0,1] | |
| double | 64 | An IEEE-754 floating-point value | `:double` |
| clampd | 64 | An IEEE-754 floating-point value, clamped to the range [0,1] | |

**Notes**:

* *ptrbits​* is the bitdepth of a CPU pointer address. Therefore, these types must be large enough to store a pointer: `sizeof(void*)`.
* Some of these types have the same internal representation as others. For example, a `sizei` is functionally equivalent to `uint`. However, the typename conveys a semantic meaning: `sizei` is used specifically for sizes of things. Similarly, `clampf` is just a GLfloat as far as C/C++ is concerned. However, when `clampf` appears in as a function argument, this means that the function will clamp this parameter to the [0,1] range. As a return type, it means the value won't exceed that range.

## Other primitive types

| Type | Same as |
| char | byte |
| char-arb | byte |
| handle-arb | uint |
| void | :void (CFFI) |
| string | :string (CFFI) |
| ptrdiff-t | :long or :long-long (CFFI) |
| intptr | ptrdiff-t |
| intptr-arb | ptrdiff-t |
| sizeiptr-arb | ptrdiff-t |
| half-arb | half |
| half-nv | half |
| clampx | int |
| egl-image-oes | (:pointer :void) (CFFI) |
| int64-ext | int64 |
| uint64-ext | uint64 |
| vdpau-surface-nv | intptr |
| debugproc | :pointer (CFFI) |
| debugproc-arb | :pointer (CFFI) |
| debugproc-amd | :pointer (CFFI) |
| debugprockhr | :pointer (CFFI) |
| _cl_context | :_cl_context (CFFI) |
| _cl_event | :_cl_event (CFFI) |