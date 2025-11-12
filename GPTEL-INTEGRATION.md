# gptel Integration with mcp-server-lib

This document describes the integration between mcp-server-lib and gptel, enabling seamless use of gptel's tool definitions as MCP server tools.

## Overview

mcp-server-lib now supports gptel's tool schema format, allowing you to:

1. **Use gptel tool definitions** - Define tools once using `gptel-make-tool` and expose them via MCP
2. **Consistent JSON handling** - Both packages use the same JSON encoding semantics
3. **Simplified tool registration** - Register gptel tools directly without conversion

## Key Changes

### 1. JSON Encoding Compatibility

The library now uses gptel's JSON encoding macros for consistent handling of:
- Empty objects: `'()` → `{}`
- Null values: `:null` → `null`
- Boolean false: `:json-false` → `false`

This ensures compatibility between gptel's LLM requests and MCP's JSON-RPC protocol.

### 2. Tool Schema Format

mcp-server-lib now accepts gptel's plist-based tool argument specifications:

```elisp
;; gptel format (now supported by mcp-server-lib)
:args (list '(:name "location"
             :type string
             :description "City name"
             :optional nil)
        '(:name "unit"
             :type string
             :description "Temperature unit"
             :optional t
             :enum ["celsius" "fahrenheit"]))
```

### 3. Removed Docstring Parsing

The library no longer parses tool parameters from function docstrings. Instead, use the explicit `:args` parameter with gptel's schema format.

**Before:**
```elisp
(defun my-tool (path)
  "Tool description.

MCP Parameters:
  path - description here"
  ...)

(mcp-server-lib-register-tool #'my-tool
  :id "tool-id"
  :description "Tool description")
```

**After:**
```elisp
(mcp-server-lib-register-tool #'my-tool
  :id "tool-id"
  :description "Tool description"
  :args (list '(:name "path"
               :type string
               :description "description here")))
```

## Usage Examples

### Example 1: Register gptel Tool Directly

```elisp
(require 'gptel)
(require 'mcp-server-lib)

;; Define a gptel tool
(gptel-make-tool
 :function (lambda (location unit)
            (format "Weather in %s: %s" location unit))
 :name "get_weather"
 :description "Get the current weather in a given location"
 :args (list '(:name "location"
              :type string
              :description "The city and state, e.g. San Francisco, CA")
         '(:name "unit"
              :type string
              :enum ["celsius" "fahrenheit"]
              :description "The unit of temperature"
              :optional t)))

;; Register it with MCP server
(mcp-server-lib-register-gptel-tool
 (gptel-get-tool "get_weather"))
```

### Example 2: Register Tool with gptel Schema

```elisp
(defun read-file (path encoding)
  "Read a file with specified encoding."
  (with-temp-buffer
    (let ((coding-system-for-read (intern encoding)))
      (insert-file-contents path)
      (buffer-string))))

(mcp-server-lib-register-tool #'read-file
  :id "read_file"
  :description "Read the contents of a file"
  :args (list '(:name "path"
               :type string
               :description "Absolute path to the file")
          '(:name "encoding"
               :type string
               :description "File encoding"
               :optional t
               :enum ["utf-8" "latin-1" "shift-jis"])))
```

### Example 3: Async Tool Support

For async gptel tools, the registration handles the callback automatically:

```elisp
(gptel-make-tool
 :function (lambda (callback url)
            (url-retrieve url
                         (lambda (status)
                          (funcall callback (buffer-string)))))
 :name "fetch_url"
 :description "Fetch content from a URL"
 :args (list '(:name "url"
              :type string
              :description "URL to fetch"))
 :async t)

(mcp-server-lib-register-gptel-tool
 (gptel-get-tool "fetch_url"))
```

## API Reference

### `mcp-server-lib-register-gptel-tool`

Register a gptel-tool struct as an MCP tool.

**Signature:**
```elisp
(mcp-server-lib-register-gptel-tool gptel-tool)
```

**Arguments:**
- `gptel-tool` - A gptel-tool struct created with `gptel-make-tool`

**Example:**
```elisp
(mcp-server-lib-register-gptel-tool (gptel-get-tool "tool_name"))
```

### Updated `mcp-server-lib-register-tool`

The function now accepts an `:args` parameter in gptel format:

**Signature:**
```elisp
(mcp-server-lib-register-tool handler &rest properties)
```

**Properties:**
- `:id` (required) - String identifier for the tool
- `:description` (required) - String describing what the tool does
- `:args` (optional) - List of argument specs in gptel format
- `:title` (optional) - User-friendly display name
- `:read-only` (optional) - Boolean indicating if tool modifies state

### Internal: `mcp-server-lib--convert-plist-schema-to-alist`

Converts gptel's plist-based tool argument specifications to MCP's alist JSON schema format.

**Signature:**
```elisp
(mcp-server-lib--convert-plist-schema-to-alist plist-schema)
```

This is used internally by both `mcp-server-lib-register-tool` and `mcp-server-lib-register-gptel-tool`.

## Migration Guide

### For Existing mcp-server-lib Users

If you have tools registered using docstring parsing, update them to use explicit `:args`:

1. Remove the "MCP Parameters:" section from docstrings
2. Add `:args` parameter to `mcp-server-lib-register-tool` calls
3. Format arguments using gptel's plist schema

### For gptel Users

If you've defined tools for gptel, you can now expose them via MCP:

1. Keep your existing `gptel-make-tool` definitions
2. Add `(require 'mcp-server-lib)` to your init
3. Call `mcp-server-lib-register-gptel-tool` for each tool you want to expose

## Advantages

1. **Single Tool Definition** - Define tools once, use with both gptel and MCP
2. **Type Safety** - gptel's enum support carries through to MCP
3. **Rich Schemas** - Support for complex types (arrays, objects, enums)
4. **Async Support** - Automatic handling of async tool execution
5. **Consistent Semantics** - Same JSON encoding/decoding across both systems

## Dependencies

- `gptel` >= 0.9 (added as package dependency)
- `gptel-openai` (for JSON encoding macros)

## Limitations

1. **Async Tool Handling** - Current async wrapper uses synchronous waiting. For true async support, consider using gptel's callback mechanism directly.

2. **Tool Categories** - gptel's tool category system is not exposed via MCP (MCP doesn't have this concept).

3. **Confirmation** - gptel's `:confirm` slot is not used by MCP registration (MCP clients handle confirmation independently).

## See Also

- [gptel documentation](https://github.com/karthink/gptel)
- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [mcp-server-lib README](README.org)
