  /*
  This is adapted from the compiled version of the custom element
  for Katex originally written in TypeScript, available here:
  https://github.com/justinfagnani/katex-elements
  */
  function initKatex() {
  
    const template = document.createElement('template');
    template.innerHTML = `
<link rel="stylesheet" href="katex/katex.min.css">
  <style>
    :host {
      display: inline-block;
    }
    * {
      user-select:none;
    }
  </style>
  <div id="container"></div>
  <div hidden><slot></slot></div>
`;
    class KatexExprElement extends HTMLElement {
        constructor() {
            super();
            const shadowRoot = this.attachShadow({ mode: 'open' });
            shadowRoot.appendChild(document.importNode(template.content, true));
            this._container = shadowRoot.querySelector('#container');
            this._slot = shadowRoot.querySelector('slot');
            this._slot.addEventListener('slotchange', () => this._render());
            // slotchange is not enough
            observer =  new MutationObserver( () => this._render());
            observer.observe(this, { characterData : true, subtree : true }); 
        }
    
        _render() {
            const inputText = this._slot.assignedNodes({ flatten: true }).map((n) => n.textContent).join('');
            katex.render(inputText, this._container, 
                { throwOnError : false });
            // Notify that it is rendered
            dispatchRendered(this);
        }
    }
    customElements.define('math-latex', KatexExprElement);


  
}