/**
 *
 */
package io.github.roycetech.ruleengine.converter;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import io.github.roycetech.ruleengine.converter.IntegerConverter;

/**
 * @author royce
 *
 */
public class IntConverterTest {

	/**
	 * Test method for
	 * {@link io.github.roycetech.ruleengine.converter.IntegerConverter#convert(java.lang.String)}.
	 */
	@Test
	public final void testConvert_classOK()
	{
		final Object actual = new IntegerConverter().convert("1");
		assertEquals(Integer.class, actual.getClass());
	}

}
