// Tencent is pleased to support the open source community by making RapidJSON available.
//
// Copyright (C) 2015 THL A29 Limited, a Tencent company, and Milo Yip. All rights reserved.
//
// Licensed under the MIT License (the "License"); you may not use this file except
// in compliance with the License. You may obtain a copy of the License at
//
// http://opensource.org/licenses/MIT
//
// Unless required by applicable law or agreed to in writing, software distributed
// under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

#include "unittest.h"

#include "rapidjson/tokeniser.h"

using namespace rapidjson;

#ifdef __GNUC__
RAPIDJSON_DIAG_PUSH
RAPIDJSON_DIAG_OFF(effc++)
RAPIDJSON_DIAG_OFF(float-equal)
#endif

TEST(RapidJsonTokeniser, Basics) {
  rapidjson::StringStream s("{ 12, -12, 12.4, \"key\": \"fish\" } true false [ ]");
  rapidjson::Tokeniser<rapidjson::StringStream> t(s);

  rapidjson::Token tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eStartObject, tok.type());

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eUInt32, tok.type());
  EXPECT_EQ( uint32_t(tok), 12u);

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eComma, tok.type());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eInt32, tok.type());
  EXPECT_EQ( int32_t(tok), -12);

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eComma, tok.type());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eDouble, tok.type());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eComma, tok.type());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eKey, tok.type());
  EXPECT_STREQ("key", (const char *)tok);
  EXPECT_EQ(rapidjson::SizeType(3), tok.strlen());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eString, tok.type());
  EXPECT_STREQ("fish", (const char *)tok);
  EXPECT_EQ(rapidjson::SizeType(4), tok.strlen());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eEndObject, tok.type());

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eBool, tok.type());
  EXPECT_EQ(bool(tok), true);

  tok = t.Get();
  EXPECT_EQ(bool(tok), false);

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eStartArray, tok.type());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eEndArray, tok.type());

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eFinished, tok.type());

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eFinished, tok.type());

  EXPECT_EQ(false, bool(t.Get(tok)));

  rapidjson::StringStream s2("[]");
  t.SetStream(s2);

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eStartArray, tok.type());

  tok = t.Get();
  EXPECT_EQ(rapidjson::Token::eEndArray, tok.type());

  t.Get(tok);
  EXPECT_EQ(rapidjson::Token::eFinished, tok.type());
}

TEST(RapidJsonTokeniser, Errors) {
  {
    rapidjson::StringStream s("[ ^ ]");
    rapidjson::Tokeniser<rapidjson::StringStream> t(s);

    rapidjson::Token tok = t.Get();
    EXPECT_EQ(rapidjson::Token::eStartArray, tok.type());

    t.Get(tok);
    EXPECT_EQ(rapidjson::Token::eFailed, tok.type());
    EXPECT_EQ(true, t.HasParseError());
    EXPECT_EQ(false, bool(t));
  }
}

#ifdef __GNUC__
RAPIDJSON_DIAG_POP
#endif
