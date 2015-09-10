Feature: Create scratch buffers

  Background:
    Given I bind key "C-c B" to "scratch-create"
    And   I bind key "C-c b" to "scratch-switch-to-buffer"
    And   I set scratch-default-name to "unnamed"

  Scenario: Create a non-scratch buffer with switch-to-buffer
    Given No buffers are open
    When I start an action chain
    And  I press "C-x b"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    But  "scratch-mode" should not be active
    And  The current buffer should be in "fundamental-mode"

  Scenario: Create a scratch buffer with scratch-create (without argument)
    Given No buffers are open
    When I press "C-c B"
    Then I should be in buffer "unnamed"
    And  "scratch-mode" should be active

  Scenario: Create a scratch buffer with scratch-create (with argument)
    Given No buffers are open
    When I start an action chain
    And  I press "C-u"
    And  I press "C-c B"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    And  "scratch-mode" should be active
    And  The current buffer should be in "emacs-lisp-mode"

  Scenario: Create a scratch buffer with scratch-switch-to-buffer
    Given No buffers are open
    When I start an action chain
    And  I press "C-c b"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    And  "scratch-mode" should be active
    And  The current buffer should be in "emacs-lisp-mode"

  Scenario: Create a scratch buffer with scratch-switch-to-buffer (no arg)
    Given No buffers are open
    When I start an action chain
    And  I press "M-:"
    And  I type "(scratch-switch-to-buffer nil)"
    And  I execute the action chain
    Then I should be in buffer "unnamed"
    And  "scratch-mode" should be active
    And  The current buffer should be in "fundamental-mode"

  Scenario: Create a scratch-buffer with scratch-pop-to-buffer
    Given No buffers are open
    When I start an action chain
    And  I press "M-x"
    And  I type "scratch-pop-to-buffer"
    And  I press "RET"
    And  I type "foo.el"
    And  I execute the action chain
    Then I should be in buffer "foo.el"
    And  "scratch-mode" should be active
    And  The current buffer should be in "emacs-lisp-mode"

  Scenario: Save a scratch buffer to make it normal
    Given No buffers are open
    When I start an action chain
    And  I press "C-c b"
    And  I type "foo.el"
    And  I execute the action chain
    Then "scratch-mode" should be active

    When I type "some modifications"
    And  I write the buffer to "/tmp/foo.el"
    Then "scratch-mode" should not be active
    
