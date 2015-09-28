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

TEST(Tokeniser, Basics) {
    StringStream s("{ 12, -12 } true, false");
    Tokeniser<StringStream> t(s);

    Token tok = t.Get();
    EXPECT_EQ(tok.type(), Token::eStartObject);

    tok = t.Get();
    EXPECT_EQ(tok.type(), Token::eUInt32);
    EXPECT_EQ( uint32_t(tok), 12u);

    tok = t.Get();
    EXPECT_EQ(tok.type(), Token::eInt32);
    EXPECT_EQ( int32_t(tok), -12);

    tok = t.Get();
    EXPECT_EQ(tok.type(), Token::eEndObject);

    tok = t.Get();
    EXPECT_EQ(tok.type(), Token::eBool);
    EXPECT_EQ(bool(tok), true);

    tok = t.Get();
    EXPECT_EQ(bool(tok), false);
}
#ifdef __GNUC__
RAPIDJSON_DIAG_POP
#endif
