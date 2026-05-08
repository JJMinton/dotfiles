## Behavioral Rules (Always Enforced)

- Do what has been asked; nothing more, nothing less
- NEVER save working files, text/mds, or tests to the root folder
- ALWAYS read a file before editing it
- NEVER commit secrets, credentials, or .env files
- ALWAYS update related docs after making a change
- ALWAYS check CHANGELOG.md and update after making changes if consistent with existing change logs.
- When making changes in response to a PR comment, ALWAYS reply to that comment with a concise explanation of how it was addressed.

## Version Control (Jujutsu by default)

Most projects use [Jujutsu](https://github.com/jj-vcs/jj) (`jj`). Use this instead of `git` for version
control unless prompted by  per-project instructions.

Create new revisions for each logical component of work. Use
```bash
jj new -m "CLAUDE: description of change" -A @
```

ALWAYS create a new revision if the current revision is not described as "CLAUDE: ..."

Bug fix changes should be made in small, atomic, coherent revisions that are described as "CLAUDE BUGFIX: ..."

## Sub-Agents & Agent Teams (Default Behavior)

- Use sub-agents and agent teams by default whenever tasks can be parallelized or decomposed
- Prefer `subagent_type=Explore` for broad codebase research; use `Glob`/`Grep` only for simple directed lookups
- Launch multiple agents concurrently for independent tasks — never serialize what can run in parallel
- Use isolated worktrees (`isolation: "worktree"`) when parallel agents modify code to avoid conflicts
- Delegate research, testing, code review, and implementation to appropriate specialized agents
- When executing a multi-step plan, dispatch independent steps to parallel sub-agents

## Plan Mode (Default Behavior when in Plan mode)

- Make the plan extremely concise — sacrifice grammar for concision
- At the end of each plan, list unresolved questions to answer, if any
- Always add a verification step when creating a plan
- Plan must include updating related docs after a specific change.

## File Organization (Recommended)

- NEVER save to root folder — use the directories below
- `/src` — source code files
- `/tests` — test files
- `/docs` — documentation and markdown files
- `/config` — configuration files
- `/scripts` — utility scripts
- `/examples` — example code
