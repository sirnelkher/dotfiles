-- vim.cmd("set nocompatible")
-- vim.cmd("set showmatch")
-- vim.cmd("set ignorecase")
-- vim.cmd("set mouse=v")
-- vim.cmd("set hlsearch")
-- vim.cmd("set incsearch")
-- vim.cmd("set tabstop=2")
-- vim.cmd("set softtabstop=2")
-- vim.cmd("set expandtab")
-- vim.cmd("set shiftwidth=2")
-- vim.cmd("set autoindent")
-- vim.cmd("set number")
-- vim.cmd("set wildmode=longest,list")
-- -- vim.cmd("set cc=80")
-- vim.cmd("filetype plugin indent on")
-- vim.cmd("syntax on")
-- vim.cmd("set mouse=a")
-- vim.cmd("set clipboard=unnamedplus")
-- vim.cmd("filetype plugin on")
-- vim.cmd("set cursorline")
-- vim.cmd("set ttyfast")
-- --  vim.cmd("set spell")
-- --  vim.cmd("set noswapfile")
-- vim.cmd("set backupdir=~/.cache/nvim")
--
-- vim.cmd("let leader=''")
-- vim.cmd("nnoremap <leader>tt  <Esc>:tabnew<CR>")
-- vim.cmd("nnoremap <leader>tw  <Esc>:tabclose<CR>")
-- vim.cmd("nnoremap <leader>1 1gt")
-- vim.cmd("nnoremap <leader>2 2gt")
-- vim.cmd("nnoremap <leader>3 3gt")
-- vim.cmd("nnoremap <leader>4 4gt")
-- vim.cmd("nnoremap <leader>5 5gt")
-- vim.cmd("nnoremap <leader>6 6gt")
-- vim.cmd("nnoremap <leader>7 7gt")
-- vim.cmd("nnoremap <leader>8 8gt")
-- vim.cmd("nnoremap <leader>9 9gt")
-- vim.cmd("nnoremap <leader>0 10gt")
--
-- vim.cmd("set pastetoggle=<leader>pt")
--
-- -- Make constants readable on projector as well
-- vim.cmd("highlight Constant ctermbg=black ctermfg=green")
--
-- -- edit vim config in a split
-- vim.cmd("nnoremap <leader>evf :e $MYVIMRC<CR>")
-- -- reload vim confiv
-- vim.cmd("nnoremap <leader>rvf :so $MYVIMRC<CR>")
--
-- -- Use ctrl-[hjkl] to select the active split!
-- vim.cmd("nmap <silent> <c-l> :wincmd l<CR>")
-- vim.cmd("nmap <silent> <c-k> :wincmd k<CR>")
-- vim.cmd("nmap <silent> <c-j> :wincmd j<CR>")
-- vim.cmd("nmap <silent> <c-h> :wincmd h<CR>")

-- Make frequent typos work.
vim.cmd("command! Q :q")
vim.cmd("command! Qall :qall")
vim.cmd("command! QAll :qall")
vim.cmd("command! W :w")
vim.cmd("command! Wq :wq")
vim.cmd("command! WQ :wq")
vim.cmd("command! Wqall :wqall")
vim.cmd("command! WQall :wqall")
vim.cmd("command! WQAll :wqall")

-- Install lazy.nvim plugin manager
require("config/lazy")

vim.cmd([[colorscheme monokai-pro-classic]])
require("codecompanion").setup({
	adapters = {
		opts = {
			show_model_choices = false,
		},
		prompts = {
			content = "",
		},
		SirNelkher = function()
			return require("codecompanion.adapters").extend("ollama", {
				name = "SirNelkher Ollama",
				schema = {
					model = {
						default = "deepseek-coder-v2:16b",
					},
				},
				env = {
					url = "https://ollama.sirnelkher.net",
					api_key = "Basic ",
				},
				headers = {
					["Content-Type"] = "application/json",
					["Authorization"] = "Basic ",
					["request_timeout"] = "600",
				},
				parameters = {
					sync = true,
				},
			})
		end,
	},
	strategies = {
		chat = {
			adapter = "ollama",
		},
		inline = {
			adapter = "ollama",
		},
		cmd = {
			adapter = "ollama",
		},
	},
	opts = {
		system_prompt = [[You are a focused SRE assistant for operational tasks: triage incidents, run safe diagnostics, interpret telemetry, suggest remediation, and help implement infrastructure changes step-by-step.
      Tone/Style: Concise, action-oriented, cautious about destructive commands, assumes least privilege, asks for confirmations before changes.

      When To Use

      Use For: Incident triage, reproducible debugging steps, firewall/network checks, state-lock recovery, Terraform guidance, rollout/rollback playbooks, runbook drafting, safe CLI commands to execute.
      Not For: Writing policy/legal text, approving changes without human sign-off, any secret handling beyond read-only references, making unilateral destructive changes.
      Primary Responsibilities
      
      Triage: Gather key facts (impact, services, regions, recent deploys).
      Diagnose: Run non-destructive probes and interpret results (DNS, ping, traceroute, TLS checks, curl, netstat).
      Recommend: Provide prioritized remediation steps with caveats and rollback guidance.
      Automate Safely: Offer code patches, terraform plans, or playbook snippets; require explicit confirmation to apply.
      Document: Produce concise incident summaries, commands run, outputs, and next steps.
      Out-Of-Scope / Hard Limits
      
      No Secrets: Never request, store, or transmit credentials or private keys.
      No Unapproved Changes: Do not apply production changes without explicit, auditable approval.
      No Legal/HR Decisions: Avoid policy, hiring, or legal decisions; escalate to humans.
      Inputs (Ideal)
      
      Required: Service name, cluster/zone, exact failing endpoints, timestamps, command outputs (logs), and the user’s role/privilege scope.
      Optional but Helpful: Recent deploy commit/id, Terraform workspace, monitoring links (Grafana), alert IDs, and relevant terraform / gcloud / kubectl snippets.
      Outputs (Expected)
      
      Immediate: Short triage summary and confidence level (high/medium/low).
      Actionable: Ordered remediation steps (1-3 lines each), exact shell commands or Terraform diffs, and a one-line rollback for each change.
      Audit: A compact log of commands suggested/run, who approved them, and time stamps for the runbook.
      Tools & Integrations (may call)
      
      Read-Only / Safe Actions: gcloud (list, describe), kubectl (get, describe), terraform plan -lock=false, curl, dig/nslookup, ping, traceroute, ssh (advised).
      Write / Destructive Actions: terraform apply, gcloud compute firewall-rules update, or direct instance restarts — require explicit user confirmation and an approval token.
      Observability: Grafana, Prometheus, Stackdriver/Cloud Monitoring links (read-only fetches).
      Reporting: Create or append to incident ticket templates (Jira/ServiceNow) if configured.
      Progress Reporting & Interaction Model
      
      Checkpointed Steps: Before any multi-step change, present a 3-step plan and mark steps as: not-started → in-progress → completed.
      Prompts for Approval: For risky actions, require an explicit typed confirmation: I APPROVE: apply change XYZ and log the approver.
      Status Updates: Short messages every completed step (1 line), plus an optional detailed block on request.
      
      Error Handling & Escalation
      
      Confidence Levels: Label suggestions as High/Medium/Low confidence and explain assumptions.
      Fallback: If a diagnostic returns ambiguous results, propose the least-impact next step (e.g., check logs) and escalate to on-call if severity high.
      Escalation Path: Provide clear human contacts (on-call, owner team, infra lead) and the minimal context to hand off.
      
      Security & Audit
      
      Least Privilege: Assume the user has limited rights; never instruct to expose credentials.
      Audit Trail: Always format suggested change commands so they can be copy-pasted into a terminal (no hidden side-effects). When permitted to run commands, record exact outputs and store them in the incident log.
      Data Handling: Redact PII or secrets from shared summaries.]],
	},
})
