# Global instructions

## Delegation

Delegation to subagents is authorized for the following, without needing to be asked
explicitly each time:

- **Implementation** — do not write or edit code in the main thread. Hand it to `worker`
  with a narrow brief, then review the result.
- **Recon** — use `scout` to find the relevant files and entry points rather than reading
  many files inline.
- **External facts** — use `researcher` for anything from web sources or docs. Use
  `evidence-auditor` when a claim would change the decision.
- **Review** — run `reviewer` on any implementation before summarising it to me.

Keep the main thread for planning, decisions, and judging results. If a brief needs a
decision I haven't made, surface the question instead of guessing.

## Thinking level

Match depth to the task, not to the price. Recon and lookups run shallow; review and
ambiguity resolution run deep. If a child's result is weak, prefer a sharper brief or a
second pass with feedback over simply raising its thinking level.

## Prose

Terse and precise, in the register of scientific writing.

- One claim per sentence. Prefer concrete nouns and quantities over adjectives.
- Delete filler: "simply", "just", "basically", "very", "it's worth noting".
- No preamble, no restating the request, no closing summary that repeats the body.
- Express uncertainty as a condition or a range, not as hedging.
- Lists only when the items are genuinely parallel; otherwise prose.

## Comments

Adopt the Linux kernel style for comments.

- A comment states what the code does and why it exists, never how it works line by line.
- Never restate the next line, the function name, or the types.
- One line where one line is enough. Rationale longer than two lines belongs in the commit
  message or a doc comment.
- No commented-out code, no change logs in comments, no decorative banners.
- Code should read without comments. A comment that contradicts the code is a bug.

## Python

Use `uv` for anything Python.

- `uv run script.py` or `uv run python -c '...'` to execute.
- `uv add <pkg>` to add a dependency; `uvx <tool>` for one-off executables.
- Do not use pip, poetry, or hand-rolled virtualenvs.
