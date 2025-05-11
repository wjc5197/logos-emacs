;; -*- lexical-binding: t; -*-
(use-package
 tabspaces
 :straight t
 :if (display-graphic-p)
 :hook
 (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
 :commands (tabspaces-switch-or-create-workspace tabspaces-open-or-create-project-and-workspace)
 :custom
 ;; (tabspaces-use-filtered-buffers-as-default t)
 (tabspaces-default-tab "⊥")
 (tabspaces-remove-to-default t)
 ;; (tabspaces-include-buffers '("*scratch*"))
 (tabspaces-initialize-project-with-todo t)
 (tabspaces-todo-file-name "project-todo.org")
 ;; sessions
 (tabspaces-session t)
 (tabspaces-session-auto-restore t)
 :config
 ;; Filter Buffers for Consult-Buffer
 (with-eval-after-load 'consult
   ;; hide full buffer list (still available with "b" prefix)
   (consult-customize consult--source-buffer :hidden t :default nil)
   ;; set consult-workspace buffer list
   (defvar +consult--source-workspace
     (list
      :name "Workspace Buffers"
      :narrow ?w
      :history 'buffer-name-history
      :category 'buffer
      :state #'consult--buffer-state
      :default t
      :items
      (lambda ()
        (consult--buffer-query
         :predicate #'tabspaces--local-buffer-p
         :sort 'visibility
         :as #'buffer-name)))

     "Set workspace buffer list for consult-buffer.")
   (add-to-list 'consult-buffer-sources '+consult--source-workspace)

   (add-hook!
    tabspaces-mode-hook
    (defun +consult-toggle-tabspaces-buffer ()
      "Deactivate isolated buffers when not using tabspaces."
      (cond
       (tabspaces-mode
        ;; hide full buffer list (still available with "b")
        (consult-customize consult--source-buffer :hidden t :default nil)
        (add-to-list 'consult-buffer-sources '+consult--source-workspace))
       (t
        ;; reset consult-buffer to show all buffers
        (consult-customize consult--source-buffer :hidden nil :default t)
        (setq consult-buffer-sources
              (remove #'+consult--source-workspace consult-buffer-sources))))))))
