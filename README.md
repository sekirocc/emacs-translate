# emacs-translate

Translate the current buffer via an LLM into a target language; the result is shown in a buffer named **\*Translation-\<current buffer name\>\*** (e.g. `*Translation-main.el*` or `*Translation-*scratch**`).

Uses **[gptel](https://github.com/karthink/gptel)** for all LLM interaction: backends, API keys, and models are configured in gptel.

## Features

- **Interactive language choice**: After running the command, a target language list (completing-read) is shown with completion; first item is the default.
- **Default English**: The first item in the list is "English"; press Enter to accept it.
- **gptel integration**: Uses gptel’s backends (OpenAI, Anthropic, Ollama, etc.) and your existing gptel configuration.

## Install and configure

1. Install **gptel** (e.g. `M-x package-install RET gptel RET`) and set up your backend and API key in gptel (see [gptel documentation](https://gptel.org)).

2. Put `emacs-translate.el` on your load-path or add its directory:

   ```elisp
   (add-to-list 'load-path "/path/to/emacs-translate")
   (load "emacs-translate")
   ```

3. No extra API keys or URLs in this package: use gptel’s `gptel-backend`, `gptel-model`, `gptel-api-key`, etc.

### gptel setup (configure backend first)

You must **register a backend in your init** (and optionally set it as the default) before `emacs-translate-buffer` and other gptel commands can send requests.

- **Register a backend**: use `gptel-make-anthropic`, `gptel-make-openai`, etc., to create a backend object.
- **Set as default**: `(setq gptel-backend <that-object>)` so all gptel requests (including this package’s translation) use it.
- **API key**: pass it as `:key "your-key"` in `gptel-make-*`, or use `gptel-api-key` / auth-source; see [gptel docs](https://gptel.org).

**Custom / third-party Anthropic-compatible endpoint** (e.g. Zhipu AI; `:host` must be the hostname only—no `https://` or path):

```elisp
(setq gptel-backend
      (gptel-make-anthropic "Claude"
                           :host "open.bigmodel.cn"
                           :endpoint "/api/anthropic/v1/messages"
                           :stream t
                           :models '(claude-sonnet-4-5-20250929)
                           :key "your-api-key"))
```

Adjust `:endpoint` and `:models` to match your provider’s documentation.

**Official Anthropic example**:

```elisp
(setq gptel-backend (gptel-make-anthropic "Claude"
                 :stream t
                 :key "your-anthropic-api-key"))
```

## Usage

- **M-x emacs-translate-buffer**  
  Translate the whole current buffer. You are prompted for the target language (default English). The translation opens in **\*Translation-\<current buffer name\>\*** (view-mode, read-only). Bind a key if desired, e.g. `(global-set-key (kbd "C-c C-t") #'emacs-translate-buffer)`.

## Dependencies

- Emacs 27.1+
- **gptel** (package dependency)

## Customize language list

Edit `emacs-translate-language-alist`: list of `(display-name . prompt-name)`; first is default:

```elisp
(setq emacs-translate-language-alist
      '(("English" . "English")
        ("Simplified Chinese" . "Simplified Chinese")
        ;; ...
        ))
```
