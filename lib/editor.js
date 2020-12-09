//Define Flight Rule Spec Language Mode
CodeMirror.defineSimpleMode('flightrule', {
  // The start state contains the rules that are intially used
  start: [
    // The regex matches the token, the token property contains the type
    { regex: /"(?:[^\\]|\\.)*?(?:"|$)/, token: 'string' },
    // Rules are matched in the order in which they appear, so there is
    // no ambiguity between this one and the one above
    {
      regex: /(?:WHEN|IF|THEN|AND|OR|NOT|EVENTUALLY|GLOBALLY|UNTIL|FOR|WITHIN|HOLDS|MS|S|M|H|D|Y|->|IS|AU|AMP|DEG)\b/,

      token: 'keyword',
    },
    { regex: /TRUE|FALSE|true|false|null|undefined/, token: 'atom' },
    {
      regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i,
      token: 'number',
    },
    { regex: /\/\/.*/, token: 'comment' },
    { regex: /\/(?:[^\\]|\\.)*?\//, token: 'variable-3' },
    // A next property will cause the mode to move to a different state
    { regex: /\/\*/, token: 'comment', next: 'comment' },
    { regex: /[-+\/*=<>!]+/, token: 'operator' },
    // indent and dedent properties guide autoindentation
    { regex: /[\{\[\(]/, token: 'grouping', indent: true },
    { regex: /[\}\]\)]/, token: 'grouping', dedent: true },
    { regex: /([a-zA-Z\d]+(?:_[a-zA-Z\d]+)*)/, token: 'variable' },
  ],
  // The multi-line comment state.
  comment: [
    { regex: /.*?\*\//, token: 'comment', next: 'start' },
    { regex: /.*/, token: 'comment' },
  ],
  // The meta property contains global information about the mode. It
  // can contain properties like lineComment, which are supported by
  // all modes, and also directives like dontIndentStates, which are
  // specific to simple modes.
  meta: {
    dontIndentStates: ['comment'],
    lineComment: '//',
  },
})

// Define the custom editor element with config, link to Elm msg
customElements.define(
  'code-editor',
  class extends HTMLElement {
    constructor() {
      super()
      this._editorValue = ''
    }

    get editorValue() {
      return this._editorValue
    }

    set editorValue(value) {
      if (this._editorValue === value) return
      this._editorValue = value
      if (!this._editor) return
      this._editor.setValue(value)
    }

    connectedCallback() {
      this._editor = CodeMirror(this, {
        indentUnit: 2,
        mode: 'flightrule',
        lineNumbers: true,
        lineWrapping: true,
        theme: 'default',
        matchBrackets: true,
        autoCloseBrackets: true,
        extraKeys: {
          'Cmd-Enter': () => {
            this.dispatchEvent(new CustomEvent('parse'))
          },
          Tab: 'autocomplete',
        },
        value: this._editorValue,
      })

      this._editor.on('changes', () => {
        this._editorValue = this._editor.getValue()
        this.dispatchEvent(new CustomEvent('editorChanged'))
      })
    }
  },
)
