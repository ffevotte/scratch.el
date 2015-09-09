Feature: Prompt to save scratch buffers

  Scenario: kill scratch buffer
    Given I kill all buffers
    And   I create a new buffer
    And   I activate "scratch-mode"
    And   I type "foo"
    And   I kill the current buffer
    Then  I should be prompted to save the buffer

  Scenario: kill non-scratch buffer
    Given I kill all buffers
    And   I create a new buffer
    And   I type "foo"
    And   I kill the current buffer
    Then  I should not be prompted to save the buffer

  Scenario: exit emacs with modified scratch buffer
    Given I kill all buffers
    And   I create a new buffer
    And   I activate "scratch-mode"
    And   I type "foo"
    And   I exit emacs
    Then  I should be prompted to save the buffer

  Scenario: exit emacs with modified non-scratch buffer
    Given I kill all buffers
    And   I create a new buffer
    And   I type "foo"
    And   I exit emacs
    Then  I should not be prompted to save the buffer
