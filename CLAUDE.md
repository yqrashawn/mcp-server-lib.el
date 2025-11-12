# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs Lisp library for building Model Context Protocol (MCP) servers. It provides infrastructure for exposing Emacs functionality as tools and resources to Large Language Models via the MCP protocol over JSON-RPC 2.0.

User-facing documentation is in README.org and in NEWS.

This is an Elisp project, and should follow user's Elisp guidelines at @~/.claude/CLAUDE-elisp.md. Also refer to common user's guidelines at ~/.claude/CLAUDE.md locally or <https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md>.

## Development Commands

### Building and Testing

```bash
# Clean, build, and run all tests
eask clean all
eask package
eask install
eask compile
eask lint elisp-lint
eask lint keywords
eask lint regexps
eask run script test

# Run only tests
eask run script test

# Run org-lint on documentation
eask run script org-lint
```

### Running Individual Tests

Tests use ERT (Emacs Lisp Regression Testing). To run a specific test:

```elisp
;; In Emacs, load the test file and run:
(ert "test-name-pattern")
```

Test files are named with `-test.el` suffix:
- `mcp-server-lib-test.el` - Main library tests
- `mcp-server-lib-bytecode-handler-test.el` - Bytecode handler tests

## Architecture Overview

### Core Components

The library is structured across multiple files:

1. **mcp-server-lib.el** (~1300 lines) - Main library
   - JSON-RPC 2.0 message handling (using gptel's encoding semantics)
   - Tool registration and dispatch
   - Resource registration (static and templated)
   - URI template matching (RFC 6570 subset)
   - Parameter validation and conversion (camelCase ↔ kebab-case)
   - gptel tool integration support

2. **mcp-server-lib-metrics.el** - Usage statistics
   - Tracks calls and errors per operation
   - Metrics stored in hash table: `mcp-server-lib-metrics--table`

3. **mcp-server-lib-commands.el** - User-facing commands
   - `mcp-server-lib-start/stop`
   - `mcp-server-lib-install/uninstall`
   - `mcp-server-lib-show-metrics/reset-metrics`

4. **mcp-server-lib-ert.el** - Testing utilities
   - ERT helpers for testing MCP servers
   - Metrics tracking macros
   - Response validation functions

5. **emacs-mcp-stdio.sh** - Stdio transport wrapper
   - Bridges MCP clients to Emacs daemon via emacsclient

### Key Data Structures

```elisp
;; Tools registry: hash table mapping tool IDs to handlers
mcp-server-lib--tools

;; Direct resources: hash table mapping URIs to handlers
mcp-server-lib--resources

;; Resource templates: list of (pattern . handler) pairs
mcp-server-lib--resource-templates

;; Server state
mcp-server-lib--running  ; boolean
```

### Reference Counting

Both tools and resources use reference counting in their registration to support multiple registrations of the same item. See `mcp-server-lib--ref-counted-register` and `mcp-server-lib--ref-counted-unregister` at mcp-server-lib.el:228-254.

### Resource Template Matching

Resource templates support RFC 6570 subset:
- `{variable}` - Simple variable expansion
- `{+variable}` - Reserved expansion (allows slashes)

Template matching is non-greedy (first-match). Direct resources take precedence over templates.

### Tool Handler Requirements

- Must return strings or nil (converted to empty string)
- Other return types cause "Invalid Params" error
- Parameters are automatically extracted from JSON and passed as function arguments
- Parameter names are converted from camelCase to kebab-case

### gptel Integration

As of version 0.2.0, mcp-server-lib integrates with gptel:

- Uses gptel's JSON encoding macros (`mcp-server-lib--json-encode`)
- Supports gptel's plist-based tool schema format
- Can register gptel-tool structs directly via `mcp-server-lib-register-gptel-tool`
- No longer parses tool parameters from function docstrings
- See GPTEL-INTEGRATION.md for detailed integration guide

## Important Development Notes

### ELPA Isolation Issue

A second copy of this package is present in user ELPA directory too, and Eask does not isolate it well. Before any Elisp work, move the ELPA copy away, and restore it after the work is completed.

### Protocol Compliance

- Follows MCP protocol version "2025-03-26"
- JSON-RPC 2.0 compliant
- Uses standard JSON-RPC error codes (defined as public constants)

### Testing Patterns

Use `mcp-server-lib-ert` utilities for testing:
- `mcp-server-lib-ert-with-server` - Run tests with server lifecycle
- `mcp-server-lib-ert-verify-req-success` - Verify successful single method calls
- `mcp-server-lib-ert-call-tool` - High-level tool testing helper
- `mcp-server-lib-ert-with-metrics-tracking` - Track metrics changes during test
