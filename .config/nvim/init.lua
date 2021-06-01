-- packer {
	local execute = vim.api.nvim_command
	local fn = vim.fn

	local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
		execute 'packadd packer.nvim'
	end

	require('packer').startup(function()
		use 'wbthomason/packer.nvim'
		use 'sainnhe/gruvbox-material'
		use 'sheerun/vim-polyglot'
		use 'neovim/nvim-lspconfig'
		use 'hrsh7th/nvim-compe'
		use 'chrisbra/matchit'
		use 'airblade/vim-gitgutter'
		use 'airblade/vim-rooter'
		use 'spf13/vim-autoclose'
		use 'tpope/vim-commentary'
		use 'tpope/vim-surround'
		use 'tpope/vim-repeat'
		use 'tpope/vim-vinegar'
	end)
-- }

-- completion {
	require('compe').setup {
		enabled = true;
		autocomplete = true;
		debug = false;
		min_length = 1;
		preselect = 'enable';
		throttle_time = 80;
		source_timeout = 200;
		incomplete_delay = 400;
		max_abbr_width = 100;
		max_kind_width = 100;
		max_menu_width = 100;
		documentation = true;

		source = {
			path = true;
			nvim_lsp = true;
		};
	}

	local t = function(str)
		return vim.api.nvim_replace_termcodes(str, true, true, true)
	end

	local check_back_space = function()
			local col = vim.fn.col('.') - 1
			if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
					return true
			else
					return false
			end
	end

	_G.tab_complete = function()
		if vim.fn.pumvisible() == 1 then
			return t "<C-n>"
		elseif check_back_space() then
			return t "<Tab>"
		else
			return vim.fn['compe#complete']()
		end
	end
	_G.s_tab_complete = function()
		if vim.fn.pumvisible() == 1 then
			return t "<C-p>"
		else
			return t "<S-Tab>"
		end
	end

	vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
	vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
	vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
	vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
	vim.o.completeopt = "menuone,noselect"
-- }

-- rust-analyzer {
	require('lspconfig').rust_analyzer.setup{}
-- }

-- better defaults {
	vim.g.mapleader = " "
	vim.o.autochdir = true
	vim.o.background = "dark"
	vim.o.clipboard = "unnamedplus"
	vim.o.expandtab = true
	vim.o.hidden = true
	vim.o.ignorecase = true
	vim.o.lazyredraw = true
	vim.o.mouse = "a"
	vim.o.wildignore = "*target/*,*.git/*,Cargo.lock"
	vim.o.path = "**"
	vim.wo.number = true
	vim.wo.relativenumber = true
	vim.o.pumheight = 10
	vim.o.shiftwidth = 4
	vim.o.showcmd = true
	vim.o.showtabline = 4
	vim.o.signcolumn = "yes"
	vim.o.smartcase = true
	vim.o.smarttab = true
	vim.o.switchbuf = "usetab"
	vim.o.tabstop = 4
	vim.o.termguicolors = true
	vim.o.wildmenu = true
	vim.o.autoread = true
	vim.o.t_Co = "256"
	vim.o.syntax = "on"
-- }

-- other {
vim.cmd([[
	set formatoptions-=cro
	set nobackup
	set noshowmode
	set noswapfile
	set nowrap
	set nowritebackup

	map Q :qa!<CR>

	let g:rooter_silent_chdir = 1
	let g:rooter_change_directory_for_non_project_files = 'current'

	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

	noremap 0 ^

	colorscheme gruvbox-material
	highlight SignColumn guibg=NONE

	nnoremap <silent> <ESC> :noh<CR><ESC>

	let g:autoclose_vim_commentmode = 1

	nmap <Leader>f :find<space>*
	nmap <Leader>o :e ~/.config/nvim/init.lua<CR>
	nmap <Leader>t :tabnew<CR>:term<CR>i
	nmap <C-h> <C-w>h
	nmap <C-j> <C-w>j
	nmap <C-k> <C-w>k
	nmap <C-l> <C-w>l
	imap < <><Left>

	nmap <Leader>1 1gt
	nmap <Leader>2 2gt
	nmap <Leader>3 3gt
	nmap <Leader>4 4gt

	au TermOpen * tnoremap <buffer> <Esc> <c-\><c-n>
]])
-- }
