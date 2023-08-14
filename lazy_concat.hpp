#pragma once

#include <type_traits>
#include <concepts>
#include <string>
#include <string_view>
#include <algorithm>
#include <numeric>
#include <ranges>

template <typename CharT, typename Traits = std::char_traits<CharT>>
class basic_lazy_concat_t;

namespace detail
{
    template <
        typename CharT,
        typename Traits
    >
    class string_view_concat_wrapper;

    template <
        typename CharT,
        typename Traits,
        typename LeftExpression
    >
    class lazy_concat_char_append;

    template <
        typename CharT,
        typename Traits,
        typename RightExpression
    >
    class lazy_concat_char_prepend;

    template <
        typename CharT,
        typename Traits,
        typename LeftExpression,
        typename RightExpression
    >
    class lazy_strings_concat;

    template <
        typename CharT,
        typename Traits,
        typename Derived
    >
    struct lazy_concat_interface
    {
        using traits_type = Traits;
        using value_type = CharT;

        constexpr const Derived& as_derived() const noexcept
        {
            return static_cast<const Derived&>(*this);
        }

        [[nodiscard]]
        constexpr std::size_t size() const noexcept
        {
            return as_derived().size();
        }

        template <typename OutputIt>
        constexpr OutputIt parse_to(OutputIt out) const noexcept
        {
            return as_derived().parse_to(std::move(out));
        }

        template <typename Allocator = std::allocator<CharT>>
        [[nodiscard]]
        constexpr auto to_string(const Allocator& alloc = {}) const
            -> std::basic_string<CharT, Traits, Allocator>
        {
            const std::size_t resulting_size = size();

            std::basic_string<CharT, Traits, Allocator> result{ alloc };

            result.resize_and_overwrite(resulting_size, [this, &resulting_size](CharT* buffer, std::size_t)
                {
                    parse_to(buffer);
                    return resulting_size;
                });

            return result;
        }

        constexpr friend std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const lazy_concat_interface& lhs)
        {
            os << lhs.as_derived();
            return os;
        }

        explicit(false) constexpr operator std::basic_string<CharT, Traits>() const
        {
            return to_string();
        }

        [[nodiscard]]
        friend constexpr auto operator+(const lazy_concat_interface& lhs, CharT c) noexcept
        {
            using result_t = detail::lazy_concat_char_append<
                CharT, Traits, Derived>;

            return result_t{ lhs.as_derived(), std::move(c) };
        }

        [[nodiscard]]
        friend constexpr auto operator+(CharT c, const lazy_concat_interface& rhs) noexcept
        {
            using result_t = detail::lazy_concat_char_prepend<
                CharT, Traits, Derived>;

            return result_t{ std::move(c), rhs.as_derived() };
        }

        [[nodiscard]]
        friend constexpr auto operator+(const lazy_concat_interface& lhs,
            std::basic_string_view<CharT, Traits> sv) noexcept
        {
            using result_t = lazy_strings_concat<
                CharT, Traits, Derived,
                string_view_concat_wrapper<CharT, Traits>>;

            return result_t{ lhs.as_derived(), string_view_concat_wrapper<CharT, Traits>{ sv } };
        }

        [[nodiscard]]
        friend constexpr auto operator+(std::basic_string_view<CharT, Traits> sv,
            const lazy_concat_interface& rhs) noexcept
        {
            using result_t = lazy_strings_concat<
                CharT, Traits,
                string_view_concat_wrapper<CharT, Traits>,
                Derived>;

            return result_t{ string_view_concat_wrapper<CharT, Traits>{ sv }, rhs.as_derived() };
        }

        [[nodiscard]]
        friend constexpr const Derived& operator+(const lazy_concat_interface& lhs, basic_lazy_concat_t<CharT, Traits>) noexcept
        {
            return lhs.as_derived();
        }

        [[nodiscard]]
        friend constexpr const Derived& operator+(basic_lazy_concat_t<CharT, Traits>, const lazy_concat_interface& rhs) noexcept
        {
            return rhs.as_derived();
        }
    };

    template <
        typename CharT,
        typename Traits,
        typename LeftExpression,
        typename RightExpression
    >
    class lazy_strings_concat
        : public lazy_concat_interface<CharT, Traits,
        lazy_strings_concat<CharT, Traits, LeftExpression, RightExpression>>
    {
    public:

        constexpr lazy_strings_concat(
            LeftExpression leftExpression,
            RightExpression rightExpression
        ) noexcept
            : leftExpression_{ leftExpression }
            , rightExpression_{ rightExpression }
        {}

        constexpr std::size_t size() const noexcept
        {
            return leftExpression_.size() + rightExpression_.size();
        }

        template <typename OutputIt>
            requires std::output_iterator<OutputIt, CharT>
        && std::contiguous_iterator<OutputIt>
            constexpr OutputIt parse_to(OutputIt out) const noexcept
        {
            out = leftExpression_.parse_to(out);
            out = rightExpression_.parse_to(out);
            return out;
        }

        friend constexpr std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const lazy_strings_concat& rhs)
        {
            os << rhs.leftExpression_;
            os << rhs.rightExpression_;
            return os;
        }

    private:
        [[msvc::no_unique_address]] LeftExpression leftExpression_;
        [[msvc::no_unique_address]] RightExpression rightExpression_;
    };

    template <
        typename CharT,
        typename Traits,
        typename LeftExpression
    >
    class lazy_concat_char_append
        : public lazy_concat_interface<CharT, Traits,
        lazy_concat_char_append<CharT, Traits, LeftExpression>>
    {
    public:
        constexpr lazy_concat_char_append(
            LeftExpression leftExpression,
            CharT c
        ) noexcept
            : leftExpression_{ leftExpression }
            , char_{ c }
        {}

        constexpr std::size_t size() const noexcept
        {
            return leftExpression_.size() + 1;
        }

        template <typename OutputIt>
            requires std::output_iterator<OutputIt, CharT>
        && std::contiguous_iterator<OutputIt>
            constexpr OutputIt parse_to(OutputIt out) const noexcept
        {
            out = leftExpression_.parse_to(std::move(out));
            Traits::assign(*out, char_);
            ++out;
            return out;
        }

        friend constexpr std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const lazy_concat_char_append& rhs)
        {
            os << rhs.leftExpression_;
            os << rhs.char_;
            return os;
        }

    private:
        [[msvc::no_unique_address]] LeftExpression leftExpression_;
        [[msvc::no_unique_address]] CharT char_;
    };

    template <
        typename CharT,
        typename Traits,
        typename RightExpression
    >
    class lazy_concat_char_prepend
        : public lazy_concat_interface<CharT, Traits,
        lazy_concat_char_prepend<CharT, Traits, RightExpression>>
    {
    public:
        constexpr lazy_concat_char_prepend(
            CharT c,
            RightExpression rightExpression
        ) noexcept
            : char_{ c }
            , rightExpression_{ rightExpression }
        {}

        constexpr std::size_t size() const noexcept
        {
            return rightExpression_.size() + 1;
        }

        template <typename OutputIt>
            requires std::output_iterator<OutputIt, CharT>
        && std::contiguous_iterator<OutputIt>
            constexpr OutputIt parse_to(OutputIt out) const noexcept
        {
            Traits::assign(*out, char_);
            ++out;
            out = rightExpression_.parse_to(std::move(out));
            return out;
        }

        friend constexpr std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const lazy_concat_char_prepend& rhs)
        {
            os << rhs.char_;
            os << rhs.rightExpression_;
            return os;
        }

    private:
        [[msvc::no_unique_address]] CharT char_;
        [[msvc::no_unique_address]] RightExpression rightExpression_;
    };

    template <typename CharT, typename Traits>
    class string_view_concat_wrapper
        : public detail::lazy_concat_interface<CharT, Traits, string_view_concat_wrapper<CharT, Traits>>
    {
    public:
        explicit constexpr string_view_concat_wrapper(std::basic_string_view<CharT, Traits> sv) noexcept
            : data_{ sv }
        {}

        [[nodiscard]]
        constexpr std::size_t size() const noexcept
        {
            return data_.size();
        }

        template <typename OutputIt>
            requires std::output_iterator<OutputIt, CharT>
        && std::contiguous_iterator<OutputIt>
            constexpr OutputIt parse_to(OutputIt out) const noexcept
        {
            Traits::copy(std::to_address(out),
                data_.data(), size());
            std::advance(out, size());
            return out;
        }

        explicit constexpr operator std::basic_string_view<CharT, Traits>() const noexcept
        {
            return data_;
        }

        friend constexpr std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const string_view_concat_wrapper& rhs)
        {
            os << rhs.data_;
            return os;
        }

    private:
        [[msvc::no_unique_address]] std::basic_string_view<CharT, Traits> data_;
    };
}

template <typename CharT, typename Traits>
class basic_lazy_concat_t : public detail::lazy_concat_interface<CharT, Traits, basic_lazy_concat_t<CharT, Traits>>
{
public:
    constexpr basic_lazy_concat_t() noexcept = default;

    [[nodiscard]]
    constexpr std::size_t size() const noexcept
    {
        return 0;
    }

    template <typename OutputIt>
        requires std::output_iterator<OutputIt, CharT>
    && std::contiguous_iterator<OutputIt>
        constexpr OutputIt parse_to(OutputIt out) const noexcept
    {
        return out;
    }

    friend constexpr std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const basic_lazy_concat_t&)
    {
        return os;
    }
};

using lazy_concat_t = basic_lazy_concat_t<char>;
using wlazy_concat_t = basic_lazy_concat_t<wchar_t>;

inline constexpr lazy_concat_t lazy_concat;
inline constexpr wlazy_concat_t wlazy_concat;
