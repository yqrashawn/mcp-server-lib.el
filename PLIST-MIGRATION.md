# Plist Migration Plan for mcp-server-lib.el

## Overview
Convert all JSON operations from alist to plist format to be consistent with gptel's JSON encoding semantics.

## Key Principles
1. All JSON data should be represented as plists internally
2. Use `plist-get` instead of `alist-get` for JSON data access
3. Use `:keyword value` syntax instead of `(key . value)` for JSON construction
4. The `mcp-server-lib--json-encode` and `mcp-server-lib--json-read` macros handle the conversion

## Files to Update

### 1. mcp-server-lib.el (Main file)

#### Core Response Functions
- `mcp-server-lib--jsonrpc-response`: Convert from `'((jsonrpc . "2.0") ...)` to `'(:jsonrpc "2.0" ...)`
- `mcp-server-lib--jsonrpc-error`: Convert error response to plist format
- `mcp-server-lib--append-optional-fields`: Rename to work with plists instead of alists

#### Request Parsing
- `mcp-server-lib--validate-and-dispatch-request`: 
  - Change `(alist-get 'jsonrpc request)` to `(plist-get request :jsonrpc)`
  - Update all parameter access from alist-get to plist-get
  
#### Handler Functions
- `mcp-server-lib--handle-resources-read`: 
  - Change `(alist-get 'uri params)` to `(plist-get params :uri)`
  
- `mcp-server-lib--handle-tools-call`:
  - Change `(alist-get 'name params)` to `(plist-get params :name)`
  - Change `(alist-get 'arguments params)` to `(plist-get params :arguments)`
  - Tool args should remain as alist since they come from params

#### Response Building
- `mcp-server-lib--handle-initialize`: Convert capability response to plist
- `mcp-server-lib--handle-tools-list`: Convert tool list entries to plist
- `mcp-server-lib--handle-resources-list`: Convert resource entries to plist
- `mcp-server-lib--execute-resource-handler`: Convert content entries to plist

#### Request Creation Functions
- `mcp-server-lib-create-tools-list-request`: Use plist format
- `mcp-server-lib-create-tools-call-request`: Use plist format
- `mcp-server-lib-create-resources-list-request`: Use plist format
- `mcp-server-lib-create-resources-read-request`: Use plist format

#### JSON Processing
- `mcp-server-lib-process-jsonrpc`: 
  - Change `json-read-from-string` to use `mcp-server-lib--json-read` macro
  - Update to expect plist format

### 2. mcp-server-lib-ert.el (Test utilities)

#### Test Helpers
- `mcp-server-lib-ert-check-text-response`: Update to use plist-get
- `mcp-server-lib-ert--validate-jsonrpc-response`: Update to use plist-get
- `mcp-server-lib-ert-assert-initialize-result`: Update to use plist-get
- `mcp-server-lib-ert-get-resource-list`: Update result access to plist-get
- `mcp-server-lib-ert-process-tool-response`: Update to use plist-get

### 3. mcp-server-lib-test.el (Tests)

All test assertions using `alist-get` need to be converted to `plist-get` or updated to expect plist format.

## Migration Strategy

### Phase 1: Core Infrastructure
1. Update `mcp-server-lib--jsonrpc-response` and `mcp-server-lib--jsonrpc-error`
2. Update `mcp-server-lib--append-optional-fields` to work with plists
3. Update `mcp-server-lib--validate-and-dispatch-request`
4. Update `mcp-server-lib-process-jsonrpc` to use plist reading

### Phase 2: Handlers
1. Update all handler functions to use plist-get for params
2. Update response construction in handlers to use plist format
3. Update request creation functions

### Phase 3: Tests
1. Update test utilities in mcp-server-lib-ert.el
2. Update test assertions in mcp-server-lib-test.el
3. Run all tests to verify correctness

## Example Conversions

### Before (alist):
```elisp
(mcp-server-lib--json-encode 
  '((jsonrpc . "2.0")
    (id . 1)
    (result . ((tools . #[])))
    ))

(let ((method (alist-get 'method request)))
  ...)
```

### After (plist):
```elisp
(mcp-server-lib--json-encode 
  '(:jsonrpc "2.0"
    :id 1
    :result (:tools #[])))

(let ((method (plist-get request :method)))
  ...)
```

## Testing Plan

1. Update core functions first
2. Run unit tests after each phase
3. Test with actual MCP clients
4. Verify JSON encoding/decoding is correct

## Notes

- The `mcp-server-lib--match-uri-template` function returns an alist of params - this is OK since it's not JSON data
- Internal data structures (tool registry, resource registry) use plists - these should remain unchanged
- Only JSON-related data structures need conversion
