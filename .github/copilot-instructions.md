# Copilot Instructions for LlmChat

Welcome to the `LlmChat` codebase! This document provides essential guidelines for AI coding agents to be productive in this project. Please follow these instructions to understand the architecture, workflows, and conventions specific to this repository.

## Project Overview

`LlmChat` is a Phoenix-based web application designed for real-time chat interactions. The application leverages Elixir's concurrency model and Phoenix's robust framework for building scalable and maintainable systems.

### Key Components

1. **Web Layer**:
   - Located in `lib/llm_chat_web/`.
   - Handles HTTP requests, routing, and rendering.
   - Uses Phoenix LiveView for interactive, real-time user interfaces.

2. **Domain Logic**:
   - Found in `lib/llm_chat/`.
   - Implements core business logic and application rules.

3. **Agents**:
   - Defined in `lib/agents/`.
   - Implements GenServer-based modules for managing stateful processes.

4. **LLM Integration**:
   - Managed in `lib/llm/`.
   - Handles communication with external large language model (LLM) APIs.

5. **Static Assets**:
   - Located in `assets/`.
   - Includes JavaScript, CSS, and Tailwind configurations.

## Developer Workflows

### Setup

1. Install dependencies:
   ```bash
   mix setup
   ```
2. Start the development server:
   ```bash
   mix phx.server
   ```

### Testing

- Run the test suite:
  ```bash
  mix test
  ```

### Debugging

- Use `IEx` for interactive debugging:
  ```bash
  iex -S mix
  ```

## Project-Specific Conventions

1. **Module Organization**:
   - Follow the Phoenix convention of separating web and domain logic.
   - Use `lib/agents/` for GenServer-based modules.

2. **Naming**:
   - Use descriptive names for modules and functions to reflect their purpose.

3. **Testing**:
   - Place test files in `test/` with a structure mirroring the `lib/` directory.

## Integration Points

- **External APIs**:
  - The application integrates with external LLM APIs, defined in `lib/llm/`.
  - Ensure API keys and secrets are managed securely.

- **Static Assets**:
  - Tailwind CSS is configured in `assets/tailwind.config.js`.

## Examples

### Adding a New Agent

1. Create a new module in `lib/agents/`:
   ```elixir
   defmodule Agents.NewAgent do
     use GenServer

     # Define GenServer callbacks here
   end
   ```

2. Add tests in `test/agents/`.

### Modifying LLM Communication

1. Update the relevant module in `lib/llm/`.
2. Ensure changes are covered by tests in `test/llm/`.

---

For more details, refer to the [Phoenix documentation](https://hexdocs.pm/phoenix/overview.html).