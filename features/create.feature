Feature: Create scratch buffers

  Background:
    Given I bind key "C-c B" to "scratch-create"
    And   I bind key "C-c b" to "scratch-switch-to-buffer"

  Scenario: scratch-create (without argument)
    Given Buffer "*scratch*<2>" does not exist
    When I press "C-c B"
    Then I should be in buffer "*scratch*<2>"
    And  "scratch-mode" should be active

  Scenario: scratch-create (with argument)
    Given Buffer "foo.el" does not exist
    When I start an action chain
    And  I press "C-u"
    And  I press "C-c B"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    And  "scratch-mode" should be active
    And  The current buffer should be in "emacs-lisp-mode"

  Scenario: scratch-switch-to-buffer
    Given Buffer "foo.el" does not exist
    When I start an action chain
    And  I press "C-c b"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    And  "scratch-mode" should be active
    And  The current buffer should be in "emacs-lisp-mode"

  Scenario: switch-to-buffer
    Given Buffer "foo.el" does not exist
    When I start an action chain
    And  I press "C-x b"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    But  "scratch-mode" should not be active
    And  The current buffer should be in "fundamental-mode"

  Scenario: scratch-pop-to-buffer
    Given Buffer "foo.el" does not exist
    When I start an action chain
    And  I press "M-x"
    And  I type "scratch-pop-to-buffer"
    And  I press "RET"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    And  "scratch-mode" should be active
    And  The current buffer should be in "emacs-lisp-mode"
