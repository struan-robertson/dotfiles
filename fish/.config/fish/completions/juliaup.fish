complete -c juliaup -n "__fish_use_subcommand" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_use_subcommand" -s V -l version -d 'Print version'
complete -c juliaup -n "__fish_use_subcommand" -f -a "default" -d 'Set the default Julia version'
complete -c juliaup -n "__fish_use_subcommand" -f -a "add" -d 'Add a specific Julia version or channel to your system. Access via `julia +{channel}` e.g. `julia +1.6`'
complete -c juliaup -n "__fish_use_subcommand" -f -a "link" -d 'Link an existing Julia binary to a custom channel name'
complete -c juliaup -n "__fish_use_subcommand" -f -a "list" -d 'List all available channels'
complete -c juliaup -n "__fish_use_subcommand" -f -a "override" -d 'Manage directory overrides'
complete -c juliaup -n "__fish_use_subcommand" -f -a "update" -d 'Update all or a specific channel to the latest Julia version'
complete -c juliaup -n "__fish_use_subcommand" -f -a "remove" -d 'Remove a Julia version from your system'
complete -c juliaup -n "__fish_use_subcommand" -f -a "status" -d 'Show all installed Julia versions'
complete -c juliaup -n "__fish_use_subcommand" -f -a "gc" -d 'Garbage collect uninstalled Julia versions'
complete -c juliaup -n "__fish_use_subcommand" -f -a "config" -d 'Juliaup configuration'
complete -c juliaup -n "__fish_use_subcommand" -f -a "api"
complete -c juliaup -n "__fish_use_subcommand" -f -a "info"
complete -c juliaup -n "__fish_use_subcommand" -f -a "self" -d 'Manage this juliaup installation'
complete -c juliaup -n "__fish_use_subcommand" -f -a "completions" -d 'Generate tab-completion scripts for your shell'
complete -c juliaup -n "__fish_use_subcommand" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from default" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from add" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from link" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from list" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "status"
complete -c juliaup -n "__fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "set"
complete -c juliaup -n "__fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "unset"
complete -c juliaup -n "__fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from status" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from set" -s p -l path -r
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from set" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from unset" -s p -l path -r
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from unset" -s n -l nonexistent
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from unset" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "status"
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "set"
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "unset"
complete -c juliaup -n "__fish_seen_subcommand_from override; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from update" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from remove" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from status" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from gc" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "channelsymlinks" -d 'Create a separate symlink per channel'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "backgroundselfupdateinterval" -d 'The time between automatic background updates of Juliaup in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "startupselfupdateinterval" -d 'The time between automatic updates at Julia startup of Juliaup in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "modifypath" -d 'Add the Julia binaries to your PATH by manipulating various shell startup scripts'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "versionsdbupdateinterval" -d 'The time between automatic updates of the versions database in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from channelsymlinks" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from backgroundselfupdateinterval" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from startupselfupdateinterval" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from modifypath" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from versionsdbupdateinterval" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "channelsymlinks" -d 'Create a separate symlink per channel'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "backgroundselfupdateinterval" -d 'The time between automatic background updates of Juliaup in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "startupselfupdateinterval" -d 'The time between automatic updates at Julia startup of Juliaup in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "modifypath" -d 'Add the Julia binaries to your PATH by manipulating various shell startup scripts'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "versionsdbupdateinterval" -d 'The time between automatic updates of the versions database in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from config; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from api" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from info" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "update" -d 'Update the Julia versions database and juliaup itself'
complete -c juliaup -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "channel" -d 'Configure the channel to use for juliaup updates'
complete -c juliaup -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "uninstall" -d 'Uninstall this version of juliaup from the system'
complete -c juliaup -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from channel" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from uninstall" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "update" -d 'Update the Julia versions database and juliaup itself'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "channel" -d 'Configure the channel to use for juliaup updates'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "uninstall" -d 'Uninstall this version of juliaup from the system'
complete -c juliaup -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c juliaup -n "__fish_seen_subcommand_from completions" -s h -l help -d 'Print help'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset" -f -a "status"
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset" -f -a "set"
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from override; and not __fish_seen_subcommand_from status; and not __fish_seen_subcommand_from set; and not __fish_seen_subcommand_from unset" -f -a "unset"
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval" -f -a "channelsymlinks" -d 'Create a separate symlink per channel'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval" -f -a "backgroundselfupdateinterval" -d 'The time between automatic background updates of Juliaup in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval" -f -a "startupselfupdateinterval" -d 'The time between automatic updates at Julia startup of Juliaup in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval" -f -a "modifypath" -d 'Add the Julia binaries to your PATH by manipulating various shell startup scripts'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from config; and not __fish_seen_subcommand_from channelsymlinks; and not __fish_seen_subcommand_from backgroundselfupdateinterval; and not __fish_seen_subcommand_from startupselfupdateinterval; and not __fish_seen_subcommand_from modifypath; and not __fish_seen_subcommand_from versionsdbupdateinterval" -f -a "versionsdbupdateinterval" -d 'The time between automatic updates of the versions database in minutes, use 0 to disable'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall" -f -a "update" -d 'Update the Julia versions database and juliaup itself'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall" -f -a "channel" -d 'Configure the channel to use for juliaup updates'
complete -c juliaup -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from channel; and not __fish_seen_subcommand_from uninstall" -f -a "uninstall" -d 'Uninstall this version of juliaup from the system'
