# Your init script
#
# Atom will evaluate this file each time a new window is opened. It is run
# after packages are loaded/activated and after the previous editor state
# has been restored.
#
# An example hack to log to the console when each text editor is saved.
#
# atom.workspace.observeTextEditors (editor) ->
#   editor.onDidSave ->
#     console.log "Saved! #{editor.getPath()}"

atom.commands.add 'atom-text-editor', 'user:toggle JSON beautify_on_save', ->
  command = 'atom-beautify.json.beautify_on_save'
  togger = atom.config.get(command)
  togger = !togger
  atom.config.set(command, togger)

atom.commands.add 'atom-text-editor', 'user:toggle Js beautify_on_save', ->
  command = 'atom-beautify.js.beautify_on_save'
  togger = atom.config.get(command)
  togger = !togger
  atom.config.set(command, togger)

atom.commands.add 'atom-text-editor', 'user:toggle use follow mode atom-focus-mode', ->
  command = 'atom-focus-mode.whenFocusModeIsActivated.useTypeWriterMode'
  togger = atom.config.get(command)
  togger = !togger
  atom.config.set(command, togger)

atom.commands.add 'atom-text-editor', 'user:toggle-show-invisible', ->
  command = 'editor.showInvisibles'
  togger = atom.config.get(command)
  togger = !togger
  atom.config.set(command, togger)

# atom.commands.add 'atom-text-editor',
#   'user:toggle-show-invisible': ->
#     param = 'editor.showInvisibles'
#     atom.config.set(param, not atom.config.get(param))
