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

atom.commands.add 'atom-text-editor', 'user:use JSON beautify_on_save', ->
  command = 'atom-beautify.json.beautify_on_save'
  atom.config.set(command, true)
  command2 = 'prettier-atom.formatOnSaveOptions.enabled'
  atom.config.set(command2, false)

atom.commands.add 'atom-text-editor', 'user:use JSON prettier-on-save', ->
  command = 'prettier-atom.formatOnSaveOptions.enabled'
  atom.config.set(command, true)
  command2 = 'atom-beautify.json.beautify_on_save'
  atom.config.set(command2, false)

atom.commands.add 'atom-text-editor', 'user:turn off format clean on save', ->
  command = 'prettier-atom.formatOnSaveOptions.enabled'
  atom.config.set(command, false)
  command2 = 'atom-beautify.json.beautify_on_save'
  atom.config.set(command2, false)
  command3 = 'atom-beautify.js.beautify_on_save'
  atom.config.set(command3, false)

atom.commands.add 'atom-text-editor', 'user:use Javascript beautify_on_save', ->
  command = 'atom-beautify.js.beautify_on_save'
  atom.config.set(command, true)

atom.commands.add 'atom-text-editor', 'user:toggle HTML beautify_on_save', ->
  command = 'atom-beautify.html.beautify_on_save'
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
