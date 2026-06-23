# Chat instructions for this repository (start here)

This file is the entrypoint for **instruction modules** that govern a reproducible chat session for this repository.

## How to start a new chat session
In your first message, specify the target GitHub repository and direct the assistant to follow these instructions.

## Instruction model used here (base + overlays)
We use a composable instruction system:

- **Base modules**: cross-cutting rules that apply to all chats (interaction protocol + quality goals).
- **Overlay modules**: domain-specific guidance that applies when relevant (e.g., Quarto books, Shiny golem apps).

Overlays are intended to be **thin** and should not duplicate the base modules.

## Selected recipe (this repository)
Selected recipe (R syntax):

```r
c("chat-manual", "goals", "development-governance", "r-package", "shiny-golem", "parameterized-help")
```

Selected modules (tokens, in order):

- chat-manual
- goals
- development-governance
- r-package
- shiny-golem
- parameterized-help

## Selected instruction modules (read in order)
Read these files in order:

1. `dev/instructions/chat-manual.md`
2. `dev/instructions/goals.md`
3. `dev/instructions/development-governance.md`
4. `dev/instructions/r-package.md`
5. `dev/instructions/shiny-golem.md`
6. `dev/instructions/parameterized-help.md`

## If the assistant cannot read repository files
If the chat platform cannot access repository files, paste the contents of:
1) this file (`CHAT_INSTRUCTIONS.md`), then
2) each of the modules listed above (in order),
into the chat.

