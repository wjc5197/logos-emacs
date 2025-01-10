;;; -*- lexical-binding: t -*-

;; GPT
(use-package
  gptel
  :straight t
  ;; :bind (("C-c a s" . gptel-send))
  :preface
  (defun +gptel-switch-backend-and-model ()
	"Switch between different backends and models for gptel."
	(interactive)
	(let* ((backend-options `(("OpenAI"    . ,openai-backend)
                              ("Gemini" . ,gemini-backend)
							  ("Deepseek"    . ,deepseek-backend)
                              ("Ollama"    . ,ollama-backend)))
           (selected-backend (completing-read "Choose backend: " (mapcar #'car backend-options)))
           (models (cond
					((string= selected-backend "OpenAI") gptel--openai-models)
					((string= selected-backend "Gemini") gptel--gemini-models)
					((string= selected-backend "Deepseek") gptel--deepseek-models)
					((string= selected-backend "Ollama") gptel--ollama-models)))
           (model-options (mapcar (lambda (model) (symbol-name (car model))) models))
           (selected-model (intern (completing-read "Choose model: " model-options)))
		   )
      (setq gptel-backend (cdr (assoc selected-backend backend-options))
			gptel-model selected-model)
      (message "Switched to backend: %s, model: %s" selected-backend selected-model)))
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (setq gemini-backend (gptel-make-gemini "Gemini" :key (auth-source-pick-first-password
														 :host "generativelanguage.googleapis.com"
														 )
										  :stream t)
		openai-backend (gptel-make-openai "ChatGPT" :key (auth-source-pick-first-password
														  :host "api.openai.com"
														  )
										  :stream t)
		ollama-backend (gptel-make-ollama "Ollama"
						 :host "localhost:11434"
						 :stream t
						 )
		deepseek-backend (gptel-make-deepseek "Deepseek" :key (auth-source-pick-first-password
															   :host "api.deepseek.com"
															   )
											  :stream t)
		gptel--deepseek-models '((deepseek-reasoner
								  :capabilities (tool reasoning)
								  :context-window 64
								  :input-cost 0.55
								  :output-cost 2.19)
								 (deepseek-chat
								  :capabilities (tool)
								  :context-window 64
								  :input-cost 0.27
								  :output-cost 1.10))
		gptel--ollma-models
		'((llama3.1
		   :description "LLaMA 3.1 model for advanced language tasks"
		   :capabilities (tool)
		   :context-window 128
		   :input-cost 1.00
		   :output-cost 5.00))
		;; default backend is gemini
		gptel-backend gemini-backend
		gptel-model 'gemini-2.0-flash
		)
  )

;; Copilot
(use-package
  copilot
  :straight t
  ;; :hook ((prog-mode . copilot-mode)
  ;;       (org-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  )
