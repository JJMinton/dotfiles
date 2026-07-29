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

## Session Workspaces (Always Enforced)

The user develops manually in the repo's default workspace. NEVER edit files there. Before the first
file edit of a session, create a dedicated jj workspace and do all work inside it.

```bash
mkdir -p ~/.claude/jj-workspaces/<repo>   # parent must exist; `jj workspace add` will not create it
jj workspace add --name claude-<slug> ~/.claude/jj-workspaces/<repo>/<slug> \
    -m "CLAUDE: description of change"
```

- `<repo>` is the repo directory basename, `<slug>` a short kebab-case name for the task.
- Run `jj workspace list` first and reuse an existing `claude-*` workspace for the same task instead
  of creating a second one.
- By default the new working-copy commit shares the parents of the default workspace's `@`, so it
  starts from the same base without inheriting the user's uncommitted changes. Pass `-r @` instead
  when the task builds on work in progress in their working copy, or `-r <rev>` to start elsewhere.
- Read/Edit/Write with absolute paths under the workspace, and `cd` into it before running commands.
  Nothing under the default workspace path may be modified.
- Workspaces added this way are NOT colocated: there is no `.git`, so `git` commands fail inside
  them. Use the `jj` equivalents (`jj st`, `jj diff`, `jj log`).
- A fresh workspace contains only tracked files. Install dependencies (e.g. `uv sync`) before running
  tests, and copy over any ignored `.env`/local config the task needs.
- The repo is shared between workspaces, so revisions made here are immediately visible to the user.
  Never rewrite (`jj edit`/`abandon`/`rebase`/`squash`) a revision they may be working on — restrict
  rewrites to revisions created this session.
- jj only snapshots the workspace a command runs in, so edits made with file tools are not in the
  revision until a `jj` command runs inside that workspace. From elsewhere the revision looks stale
  and possibly empty. Run `jj st` in the workspace before reading its state or removing it.

Keep creating a new revision per logical change, as above, inside the workspace.

Once the work is complete and described, remove the workspace and report the change id so the user
can pick the work up with `jj new <change-id>`:

```bash
cd <workspace> && jj st         # snapshot first, or the final edits are lost on forget
cd <repo> && jj workspace forget claude-<slug>
rm -rf ~/.claude/jj-workspaces/<repo>/<slug>
```

`jj workspace forget` leaves the workspace's working-copy commit in the repo even when it is empty
(jj 0.43), so `jj abandon <change-id>` any revision that ended up with no changes.

For a plain git repo with no `.jj`, use `git worktree add ~/.claude/worktrees/<repo>/<slug>` the same
way, and `git worktree remove` to clean up.

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
