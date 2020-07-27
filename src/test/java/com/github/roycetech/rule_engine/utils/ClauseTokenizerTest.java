/**
 *
 */
package com.github.roycetech.rule_engine.utils;

import static org.junit.Assert.assertArrayEquals;

import org.junit.Test;

/**
 * @author royce
 *
 */
public class ClauseTokenizerTest {

    /**
     * Test method for
     * {@link com.github.roycetech.rule_engine.utils.TokenizerUtil#tokenize(java.lang.String, java.lang.String)}.
     */
    @Test
    public final void testTokenize()
    {
	assertArrayEquals(new Object[] { "true[0]", "&", "true[1]"
	}, TokenizerUtil.tokenize("true[0]&true[1]", "&|"));
    }

}
