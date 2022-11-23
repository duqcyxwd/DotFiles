require("funcs.global")

SR("hop").setup()
DR('lspkind').init()
SR('neogen').setup {}

SR("gitlinker").setup({
  opts = {
    -- remote = nil, -- force the use of a specific remote
    -- adds current line nr in the url for normal mode
    -- add_current_line_on_normal_mode = true,
    -- callback for what to do with the url
    -- action_callback = require"gitlinker.actions".copy_to_clipboard,
    -- print the url after performing the action
    print_url = true,
  },
  callbacks = {
        ["gerrit.ericsson.se"] = function(url_data)
					local server_url = "https://gerrit.ericsson.se/plugins/gitiles/OSS/com.ericsson.oss.air/eric-oss-core-slice-assurance-cfg/+/"

					-- local url = require"gitlinker.hosts".get_base_https_url(url_data) ..
					-- url_data.repo .. "/blob/" .. url_data.rev .. "/" .. url_data.file

					local url = server_url .. url_data.rev ..  "/" .. url_data.file
					if url_data.lstart then
						url = url .. "#" .. url_data.lstart
						if url_data.lend then url = url .. "-" .. url_data.lend end
					end
					return url
				end
			},
		})
