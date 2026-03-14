# Claude Code

## Context
VP, Viewpoints | Hack, React, GraphQL, Ent, Relay, Python | www/, fbcode/

## Communication
Direct, technical, skip basics. Tables over prose. Criticize flawed ideas.

## NEVER
| Don't                                         | Do Instead                          |
|-----------------------------------------------|-------------------------------------|
| Create files without asking                   | Ask first, or only when essential   |
| Use `any`/`mixed` types                       | Fix types properly                  |
| Refactor adjacent code                        | Only touch what's needed            |
| Comment unchanged code                        | Comment only what you changed       |
| Over-engineer                                 | Solve current problem simply        |
| Submit without running all test plan commands | Run every command, confirm all pass |
| Submit without completed summary + test plan  | Finish both before `jf submit`      |
| Submit as non-draft                           | Always use `jf submit --draft`      |
| Submit without project `anna` as reviewer     | Always add project `anna`           |
| Use `jf get` to pull diffs                    | Use `sl pull` + `sl goto <commit>`  |

## ASK FIRST
- Schema/Ent changes
- Public API/GraphQL changes
- Deleting code that has tests

## Workflow

### Pre-Commit Check (before every `sl commit`)
1. `sl log -r . --template '{desc|firstline}\n'` → verify on master or related diff
2. If on unrelated diff → `sl goto master` first
3. `sl status -m -a -r` → confirm only intended files are staged
4. If unrelated files dirty → `sl shelve` or `sl revert` them first

### Development
1. `hh` / `flow` → fix type errors (Python: lint-only, no type checker)
2. `arc lint -a` → fix lint errors
3. `code-reviewer` agent → address issues
4. `arc unit` / `buck test` → fix test failures
5. `/ready` → verify gates
6. `jf submit --draft`

## Commits
**Format:** `[VP][Workstream] Verb phrase`

**Example:** `[VP][Product UX] Add chart visualization`

**Summary:** High-level what + why. No granular per-line details. If the "why"
isn't clear from conversation context, ask the user directly before committing.

**Reviewers:** Always add project `anna`.

**Split when:** Server vs client code in separate diffs. Keep diffs <500 lines.
Split by logical grouping (what's being done), not by file count.

### Test Plan
Auto-generate the test plan in commit bodies based on changed files:

| Changed files                    | Include in test plan |
|----------------------------------|----------------------|
| `*.js`, `*.jsx`, `*.ts`, `*.tsx` | `flow && arc lint`   |
| `*.php`, `*.hack` (www/)         | `hh && arc lint`     |
| `*.py`                           | `arc lint`           |

If unit tests were added or modified, also include:
- www/ → `arc unit <path>`, fbcode/ → `buck test <path>`
- The result URL from the output (if provided), otherwise paste the pass/fail summary

## Model Selection
| Agent Type                     | Model  | Why               |
|--------------------------------|--------|-------------------|
| Explore, researcher            | haiku  | Fast, cheap       |
| code-reviewer, debugger, coder | sonnet | Accuracy          |
| architect                      | opus   | Complex decisions |

## Coding Practices

### Test-First Bug Fixing
When fixing a code bug, write a failing test that reproduces the bug BEFORE
attempting a fix. Then fix the code and verify the test passes.
Skip for infra/config issues — just diagnose and fix directly.

## Common Pitfalls
| Pitfall                                   | Solution                                 |
|-------------------------------------------|------------------------------------------|
| Modifying `@generated` / `SignedSource` files | Find the source file, edit it, run the codegen command from the file header |
| Changing enums with GraphQL bindings      | Run `arc rebuild www` after              |
| Editing message blocks without both sides | Coordinate Hack + JS changes together    |
| Passing incomplete context to subagents   | Verify all required context is forwarded |

## Quick Reference
| Task               | Command                    |
|--------------------|----------------------------|
| Type check Hack    | `hh` or `hh --single path` |
| Type check JS      | `flow path`                |
| Lint & fix         | `arc lint -a`              |
| Run tests (www)    | `arc unit path`            |
| Run tests (fbcode) | `buck test path`           |
| Submit draft       | `jf submit --draft`        |
| Rebuild enums      | `arc rebuild www`          |
