/**
 *
 */
package io.github.roycetech.ruleengine.utils;

import static org.junit.Assert.assertArrayEquals;

import org.junit.Test;

import io.github.roycetech.ruleengine.utils.TokenizerUtil;

/**
 * @author royce
 *
 */
public class ClauseTokenizerTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.utils.TokenizerUtil#tokenize(java.lang.String, java.lang.String)}.
	 */
	@Test
	public final void testTokenize()
	{
		assertArrayEquals(new Object[] { "true[0]", "&", "true[1]" },
				TokenizerUtil.tokenize("true[0]&true[1]", "&|"));
	}

}
