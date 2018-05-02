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
  atom.config.set('atom-beautify.json.beautify_on_save', true)
  atom.config.set('prettier-atom.formatOnSaveOptions.enabled', false)

atom.commands.add 'atom-text-editor', 'user:use JSON prettier-on-save', ->
  atom.config.set('prettier-atom.formatOnSaveOptions.enabled', true)
  atom.config.set('atom-beautify.json.beautify_on_save', false)

atom.commands.add 'atom-text-editor', 'user:turn off format clean on save', ->
  atom.config.set('prettier-atom.formatOnSaveOptions.enabled', false)
  atom.config.set('atom-beautify.json.beautify_on_save', false)
  atom.config.set('atom-beautify.js.beautify_on_save', false)

atom.commands.add 'atom-text-editor', 'user:use Javascript beautify_on_save', ->
  atom.config.set('atom-beautify.js.beautify_on_save', true)
  atom.config.set('prettier-atom.formatOnSaveOptions.enabled', false)

atom.commands.add 'atom-text-editor', 'user:use Javascript prettier-on-save', ->
  atom.config.set('atom-beautify.js.beautify_on_save', false)
  atom.config.set('prettier-atom.formatOnSaveOptions.enabled', true)

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
