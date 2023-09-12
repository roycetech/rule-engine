package com.github.roycetech.ruleengine.utils;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class StringUtilTest {

	@Test
	public void testHasValue_null()
	{
		assertFalse(StringUtil.hasValue(null));
	}

	@Test
	public void testHasValue_empty()
	{
		assertFalse(StringUtil.hasValue(""));
	}

	@Test
	public void testHasValue_happy()
	{
		assertTrue(StringUtil.hasValue("   Test"));
	}

	@Test
	public void testtrimArray_null()
	{
		assertNull(StringUtil.trimArray((String[]) null));
	}

	@Test
	public void testtrimArray_notRequired()
	{
		assertArrayEquals(new String[] { "one" }, StringUtil.trimArray(new String[] { "one" }));
	}

	@Test
	public void testtrimArray_containsNull()
	{
		assertArrayEquals(new String[] { "three", null },
				StringUtil.trimArray(new String[] { "three   ", null }));
	}

	@Test
	public void testtrimArray_happy()
	{
		assertArrayEquals(new String[] { "two" },
				StringUtil.trimArray(new String[] { "   two  " }));
	}

}
