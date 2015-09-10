Feature: Prompt to save scratch buffers

  Scenario: Kill a non-scratch buffer => no prompt
    Given No buffers are open
    When  I create a new buffer
    And   I type "some modifications"
    And   I kill the current buffer
    Then  I should not be prompted to save the buffer

  Scenario: Kill a scratch buffer => prompt to save
    Given No buffers are open
    When  I create a new buffer
    And   I activate "scratch-mode"
    And   I type "some modifications"
    And   I kill the current buffer
    Then  I should be prompted to save the buffer
    And   The buffer should be saved to disk

  Scenario: exit emacs with a modified non-scratch buffer => no prompt
    Given No buffers are open
    When  I create a new buffer
    And   I type "some modifications"
    And   I exit emacs
    Then  I should not be prompted to save the buffer
    And   Emacs should exit

  Scenario: Exit emacs with modified scratch buffer => prompt to save
    Given No buffers are open
    When  I create a new buffer
    And   I activate "scratch-mode"
    And   I type "some modifications"
    And   I exit emacs
    Then  I should be prompted to save the buffer
    And   Emacs should not exit
